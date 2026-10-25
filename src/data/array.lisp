(in-package :cl-cc/vm)

;;; VM Array/Vector Operations
;;;
;;; This file extends the VM with array and vector instructions including
;;; construction, element access/mutation, dimension queries, fill-pointer
;;; management, bit-array operations, and displaced-array support.
;;;

;;; ─── Basic Array Instructions ────────────────────────────────────────────

(define-vm-instruction vm-make-array (vm-instruction)
  "Create an array of given size. Supports make-array keyword arguments."
  (dst nil :reader vm-dst)
  (size-reg nil :reader vm-size-reg)
  (dimensions-reg nil :reader vm-dimensions-reg)
  (initial-element nil :reader vm-initial-element)
  (fill-pointer nil :reader vm-fill-pointer)
  (fill-pointer-reg nil :reader vm-fill-pointer-reg)
  (adjustable nil :reader vm-adjustable)
  (adjustable-reg nil :reader vm-adjustable-reg)
  (element-type nil :reader vm-element-type)
  (element-type-reg nil :reader vm-element-type-reg)
  (displaced-to-reg nil :reader vm-displaced-to-reg)
  (displaced-index-offset-reg nil :reader vm-displaced-index-offset-reg)
  (copy-on-write nil :reader vm-copy-on-write)
  (copy-on-write-reg nil :reader vm-copy-on-write-reg)
  (:sexp-tag :make-array)
  (:sexp-slots dst size-reg initial-element fill-pointer adjustable element-type
                fill-pointer-reg adjustable-reg element-type-reg displaced-to-reg
                dimensions-reg displaced-index-offset-reg copy-on-write copy-on-write-reg))

(define-vm-instruction vm-aref (vm-instruction)
  "Get element at INDEX from ARRAY, store in DST."
  (dst nil :reader vm-dst)
  (array-reg nil :reader vm-array-reg)
  (index-reg nil :reader vm-index-reg)
  (:sexp-tag :aref)
  (:sexp-slots dst array-reg index-reg))

;; vm-aref-multi: N-dimensional array read (2+ indices) — custom sexp like vm-format-inst
(define-vm-instruction vm-aref-multi (vm-instruction)
  "Multi-dimensional array read. DST = (apply #'aref ARRAY INDICES...)."
  (dst nil :reader vm-dst)
  (array-reg nil :reader vm-array-reg)
  (index-regs nil :reader vm-index-regs))

(defmethod instruction->sexp ((inst vm-aref-multi))
  (list* :aref-multi (vm-dst inst) (vm-array-reg inst) (vm-index-regs inst)))

(setf (gethash :aref-multi *instruction-constructors*)
      (lambda (sexp)
        (make-vm-aref-multi :dst (second sexp)
                            :array-reg (third sexp)
                            :index-regs (cdddr sexp))))

(define-vm-instruction vm-aset (vm-instruction)
  "Set element at INDEX in ARRAY to VAL."
  (array-reg nil :reader vm-array-reg)
  (index-reg nil :reader vm-index-reg)
  (val-reg nil :reader vm-val-reg)
  (:sexp-tag :aset)
  (:sexp-slots array-reg index-reg val-reg))

(define-vm-instruction vm-fill (vm-instruction)
  "Fill ARRAY with VAL."
  (array-reg nil :reader vm-array-reg)
  (val-reg nil :reader vm-val-reg)
  (:sexp-tag :fill)
  (:sexp-slots array-reg val-reg))

(define-vm-instruction vm-copy-vector (vm-instruction)
  "Copy LEN elements from SRC-ARRAY to DST-ARRAY."
  (dst-array-reg nil :reader vm-dst-array-reg)
  (src-array-reg nil :reader vm-src-array-reg)
  (len-reg nil :reader vm-len-reg)
  (:sexp-tag :copy-vector)
  (:sexp-slots dst-array-reg src-array-reg len-reg))

(define-vm-instruction vm-vector (vm-instruction)
  "Create a simple vector from already-evaluated element registers."
  (dst nil :reader vm-dst)
  (element-regs nil :reader vm-element-regs)
  (:sexp-tag :vector)
  (:sexp-slots dst element-regs))

(define-vm-instruction vm-vector-push-extend (vm-instruction)
  "Push VAL onto adjustable ARRAY, extending if needed. Store new index in DST."
  (dst nil :reader vm-dst)
  (val-reg nil :reader vm-val-reg)
  (array-reg nil :reader vm-array-reg)
  (:sexp-tag :vector-push-extend)
  (:sexp-slots dst val-reg array-reg))

(define-vm-instruction vm-array-length (vm-instruction)
  "Get the length of array/vector in SRC, store in DST."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :array-length)
  (:sexp-slots dst src))

(define-vm-instruction vm-vectorp (vm-instruction)
  "Check if value is a vector."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :vectorp)
  (:sexp-slots dst src))

;;; ─── Execute basic array instructions ───────────────────────────────────

(defun %vm-make-displaced-array (dimensions elt-type displaced-to displaced-index-offset fp adj)
  (cond
    ((and fp adj)
     (make-array dimensions :element-type (or elt-type t)
                 :displaced-to displaced-to
                 :displaced-index-offset displaced-index-offset
                 :fill-pointer (if (eq fp t) 0 fp)
                 :adjustable t))
    (fp
     (make-array dimensions :element-type (or elt-type t)
                 :displaced-to displaced-to
                 :displaced-index-offset displaced-index-offset
                 :fill-pointer (if (eq fp t) 0 fp)))
    (adj
     (make-array dimensions :element-type (or elt-type t)
                 :displaced-to displaced-to
                 :displaced-index-offset displaced-index-offset
                 :adjustable t))
    (t
     (make-array dimensions :element-type (or elt-type t)
                 :displaced-index-offset displaced-index-offset
                 :displaced-to displaced-to))))

(defun %vm-make-standard-array (dimensions elt-type init-elem fp adj)
  (cond
    ((and fp adj)
     (make-array dimensions :element-type (or elt-type t) :initial-element init-elem
                 :fill-pointer (if (eq fp t) 0 fp)
                 :adjustable t))
    (fp
     (make-array dimensions :element-type (or elt-type t) :initial-element init-elem
                 :fill-pointer (if (eq fp t) 0 fp)))
    (adj
     (make-array dimensions :element-type (or elt-type t) :initial-element init-elem
                 :adjustable t))
    (t
     (make-array dimensions :element-type (or elt-type t) :initial-element init-elem))))

(defmethod execute-instruction ((inst vm-make-array) state pc labels)
  (declare (ignore labels))
  (let* ((size (vm-reg-get state (vm-size-reg inst)))
          (dimensions (and (vm-dimensions-reg inst)
                           (vm-reg-get state (vm-dimensions-reg inst))))
          (dimensions-designator (%vm-array-dimensions-designator size dimensions))
          (total-size (%vm-array-total-size dimensions-designator))
          (vector-dimensions-p (%vm-array-vector-dimensions-p dimensions-designator))
          (init-present-p (vm-initial-element inst))
          (fp (if (vm-fill-pointer-reg inst)
                  (vm-reg-get state (vm-fill-pointer-reg inst))
                  (vm-fill-pointer inst)))
          (adj (if (vm-adjustable-reg inst)
                   (vm-reg-get state (vm-adjustable-reg inst))
                   (vm-adjustable inst)))
          (elt-type (if (vm-element-type-reg inst)
                        (vm-reg-get state (vm-element-type-reg inst))
                        (vm-element-type inst)))
          (displaced-to (and (vm-displaced-to-reg inst)
                              (vm-reg-get state (vm-displaced-to-reg inst))))
           (displaced-index-offset (if (vm-displaced-index-offset-reg inst)
                                       (vm-reg-get state (vm-displaced-index-offset-reg inst))
                                       0))
           (copy-on-write (if (vm-copy-on-write-reg inst)
                              (vm-reg-get state (vm-copy-on-write-reg inst))
                              (vm-copy-on-write inst)))
          (default-init (case elt-type
                          (character #\Nul)
                          (single-float 0.0f0)
                         (double-float 0.0d0)
                         (bit 0)
                         (otherwise 0)))
          (init-elem (if init-present-p
                         (vm-reg-get state (vm-initial-element inst))
                         default-init))
           (specialized-type (%vm-normalize-specialized-element-type elt-type))
           (arr (cond
                     (displaced-to
                      (%vm-make-displaced-array dimensions-designator elt-type displaced-to
                                                displaced-index-offset fp adj))
                    ((and vector-dimensions-p
                          (member specialized-type '(:fixnum :double-float :character :bit)
                                   :test #'eq)
                           (not fp)
                          (not adj)
                          (not init-present-p))
                     (vm-make-specialized-array total-size specialized-type))
                   (t
                    (%vm-make-standard-array dimensions-designator elt-type init-elem fp adj)))))
    (vm-reg-set state (vm-dst inst) (if copy-on-write (%vm-cow-vector-share arr) arr))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-aref) state pc labels)
  (declare (ignore labels))
  (let ((arr (%vm-cow-vector-materialize (vm-reg-get state (vm-array-reg inst))))
        (idx (vm-reg-get state (vm-index-reg inst))))
    (unless (vm-specialized-array-p arr)
      (vm-check-index arr idx 'aref))
    (vm-reg-set state (vm-dst inst)
                (if (vm-specialized-array-p arr)
                    (vm-specialized-array-ref arr idx)
                    (aref arr idx)))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-aref-multi) state pc labels)
  (declare (ignore labels))
  (let ((arr (%vm-cow-vector-materialize (vm-reg-get state (vm-array-reg inst))))
        (idxs (mapcar (lambda (r) (vm-reg-get state r)) (vm-index-regs inst))))
    (when (vm-safety-checks-enabled-p)
      (unless (apply #'array-in-bounds-p arr idxs)
        (error 'type-error :datum idxs :expected-type 'valid-array-subscripts)))
    (vm-reg-set state (vm-dst inst) (apply #'aref arr idxs))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-aset) state pc labels)
  (declare (ignore labels))
  (let ((arr (%vm-cow-vector-ensure-writable (vm-reg-get state (vm-array-reg inst))))
        (idx (vm-reg-get state (vm-index-reg inst)))
        (val (vm-reg-get state (vm-val-reg inst))))
    (unless (vm-specialized-array-p arr)
      (vm-check-index arr idx 'aset))
    (if (vm-specialized-array-p arr)
        (setf (vm-specialized-array-ref arr idx) val)
        (setf (aref arr idx) val))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-fill) state pc labels)
  (declare (ignore labels))
  (let ((arr (%vm-cow-vector-ensure-writable (vm-reg-get state (vm-array-reg inst))))
        (val (vm-reg-get state (vm-val-reg inst))))
    (if (vm-specialized-array-p arr)
        (loop for i below (vm-specialized-array-length arr)
              do (setf (vm-specialized-array-ref arr i) val))
        (fill arr val))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-copy-vector) state pc labels)
  (declare (ignore labels))
  (let ((dst (%vm-cow-vector-ensure-writable (vm-reg-get state (vm-dst-array-reg inst))))
        (src (%vm-cow-vector-materialize (vm-reg-get state (vm-src-array-reg inst))))
        (len (vm-reg-get state (vm-len-reg inst))))
    (cond
      ((or (vm-specialized-array-p dst) (vm-specialized-array-p src))
       (loop for i below len
             for value = (if (vm-specialized-array-p src)
                             (vm-specialized-array-ref src i)
                             (aref src i))
             do (if (vm-specialized-array-p dst)
                    (setf (vm-specialized-array-ref dst i) value)
                    (setf (aref dst i) value))))
      (t
       (replace dst src :end1 len :end2 len)))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-vector) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (coerce (mapcar (lambda (reg) (vm-reg-get state reg))
                              (vm-element-regs inst))
                      'vector))
  (values (1+ pc) nil nil))

(defun %vm-bridge-replace (sequence-1 sequence-2 &key end1 end2)
  "VM-safe bridge for CL:REPLACE over ordinary, COW, and specialized arrays."
  (let ((dst (%vm-cow-vector-ensure-writable sequence-1))
        (src (%vm-cow-vector-materialize sequence-2)))
    (cond
      ((or (vm-specialized-array-p dst) (vm-specialized-array-p src))
       (let ((limit (min (or end1 (if (vm-specialized-array-p dst)
                                      (vm-specialized-array-length dst)
                                      (length dst)))
                         (or end2 (if (vm-specialized-array-p src)
                                      (vm-specialized-array-length src)
                                      (length src))))))
         (loop for i below limit
               for value = (if (vm-specialized-array-p src)
                               (vm-specialized-array-ref src i)
                               (aref src i))
               do (if (vm-specialized-array-p dst)
                      (setf (vm-specialized-array-ref dst i) value)
                      (setf (aref dst i) value)))
         sequence-1))
      (t
       (replace dst src :end1 end1 :end2 end2)
       sequence-1))))

(eval-when (:load-toplevel :execute)
  (vm-register-host-bridge 'cl:replace #'%vm-bridge-replace))

(defmethod execute-instruction ((inst vm-vector-push-extend) state pc labels)
  (declare (ignore labels))
  (let ((val (vm-reg-get state (vm-val-reg inst)))
        (arr (%vm-cow-vector-ensure-writable (vm-reg-get state (vm-array-reg inst)))))
    (vm-reg-set state (vm-dst inst) (vector-push-extend val arr))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-array-length) state pc labels)
  (declare (ignore labels))
  (let ((src (%vm-cow-vector-materialize (vm-reg-get state (vm-src inst)))))
    (vm-reg-set state (vm-dst inst)
                (if (vm-specialized-array-p src)
                    (vm-specialized-array-length src)
                    (length src)))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-vectorp) state pc labels)
  (declare (ignore labels))
  (let ((src (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst)
                (if (or (vectorp src) (vm-cow-vector-p src)
                        (vm-specialized-array-p src)) 1 0))
    (values (1+ pc) nil nil)))

;;; ─── FR-601: Array dimension queries ─────────────────────────────────────

(define-vm-instruction vm-array-rank (vm-instruction)
  "Return number of dimensions of ARRAY."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :array-rank) (:sexp-slots dst src))
(define-simple-instruction vm-array-rank :unary vm-array-rank-value)

(define-vm-instruction vm-array-total-size (vm-instruction)
  "Return total number of elements in ARRAY."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :array-total-size) (:sexp-slots dst src))
(define-simple-instruction vm-array-total-size :unary vm-array-total-size-value)

(define-vm-instruction vm-array-dimensions (vm-instruction)
  "Return list of dimension sizes of ARRAY."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :array-dimensions) (:sexp-slots dst src))
(define-simple-instruction vm-array-dimensions :unary vm-array-dimensions-value)

(define-vm-instruction vm-array-element-type (vm-instruction)
  "Return ARRAY's element type."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :array-element-type) (:sexp-slots dst src))
(define-simple-instruction vm-array-element-type :unary vm-array-element-type-value)

(define-vm-instruction vm-array-dimension (vm-instruction)
  "Return size of dimension AXIS of ARRAY."
  (dst nil :reader vm-dst) (lhs nil :reader vm-lhs) (rhs nil :reader vm-rhs)
  (:sexp-tag :array-dimension) (:sexp-slots dst lhs rhs))
(defmethod execute-instruction ((inst vm-array-dimension) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (vm-array-dimension-value (vm-reg-get state (vm-lhs inst))
                                        (vm-reg-get state (vm-rhs inst))))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-array-in-bounds-p (vm-instruction)
  "Return true if SUBSCRIPTS are valid for ARRAY."
  (dst nil :reader vm-dst) (arr nil :reader vm-arr) (subs nil :reader vm-subs)
  (:sexp-tag :array-in-bounds-p) (:sexp-slots dst arr subs))
(defmethod execute-instruction ((inst vm-array-in-bounds-p) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (if (vm-array-in-bounds-p-value (vm-reg-get state (vm-arr inst))
                                              (vm-reg-get state (vm-subs inst)))
                  t
                  nil))
  (values (1+ pc) nil nil))

;;; ─── FR-602: Row-major access ────────────────────────────────────────────

(define-vm-instruction vm-row-major-aref (vm-instruction)
  "Access array element by row-major index."
  (dst nil :reader vm-dst) (lhs nil :reader vm-lhs) (rhs nil :reader vm-rhs)
  (:sexp-tag :row-major-aref) (:sexp-slots dst lhs rhs))
(defmethod execute-instruction ((inst vm-row-major-aref) state pc labels) (declare (ignore labels)) (let ((array (%vm-array-object (vm-reg-get state (vm-lhs inst)))) (index (vm-reg-get state (vm-rhs inst)))) (vm-reg-set state (vm-dst inst) (if (vm-specialized-array-p array) (vm-specialized-array-ref array index) (progn (vm-check-row-major-index array index) (row-major-aref array index))))) (values (1+ pc) nil nil))

(define-vm-instruction vm-array-row-major-index (vm-instruction)
  "Compute the row-major index for ARRAY given a list of SUBSCRIPTS."
  (dst nil :reader vm-dst) (arr nil :reader vm-arr) (subs nil :reader vm-subs)
  (:sexp-tag :array-row-major-index) (:sexp-slots dst arr subs))
(defmethod execute-instruction ((inst vm-array-row-major-index) state pc labels)
  (declare (ignore labels))
  (let ((arr (vm-reg-get state (vm-arr inst)))
        (subs (vm-reg-get state (vm-subs inst))))
    (vm-reg-set state (vm-dst inst) (apply #'array-row-major-index (%vm-array-object arr) subs))
    (values (1+ pc) nil nil)))

;;; ─── FR-603: svref — simple-vector element access ────────────────────────

(define-vm-instruction vm-svref (vm-instruction)
  "Access element of simple-vector by index."
  (dst nil :reader vm-dst) (lhs nil :reader vm-lhs) (rhs nil :reader vm-rhs)
  (:sexp-tag :svref) (:sexp-slots dst lhs rhs))
(defmethod execute-instruction ((inst vm-svref) state pc labels)
  (declare (ignore labels))
  (let ((vector (%vm-cow-vector-materialize (vm-reg-get state (vm-lhs inst))))
        (index (vm-reg-get state (vm-rhs inst))))
    (vm-check-index vector index 'svref)
    (vm-reg-set state (vm-dst inst) (svref vector index)))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-svset (vm-instruction)
  "Set element of simple-vector by index. Returns new value."
  (dst nil :reader vm-dst) (array-reg nil :reader vm-array-reg)
  (index-reg nil :reader vm-index-reg) (val-reg nil :reader vm-val-reg)
  (:sexp-tag :svset) (:sexp-slots dst array-reg index-reg val-reg))
(defmethod execute-instruction ((inst vm-svset) state pc labels)
  (declare (ignore labels))
  (let* ((arr (%vm-cow-vector-ensure-writable (vm-reg-get state (vm-array-reg inst))))
         (idx (vm-reg-get state (vm-index-reg inst)))
         (val (vm-reg-get state (vm-val-reg inst))))
    (vm-check-index arr idx 'svset)
    (setf (svref arr idx) val)
    (vm-reg-set state (vm-dst inst) val)
    (values (1+ pc) nil nil)))

;;; ─── FR-604: fill-pointer and vector-push ────────────────────────────────

(define-vm-instruction vm-fill-pointer-inst (vm-instruction)
  "Return fill pointer of vector."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :fill-pointer-inst) (:sexp-slots dst src))
(define-simple-instruction vm-fill-pointer-inst :unary (lambda (array) (fill-pointer (%vm-array-object array))))

(define-vm-instruction vm-array-has-fill-pointer-p (vm-instruction)
  "Return T if array has a fill pointer."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :array-has-fill-pointer-p) (:sexp-slots dst src))
(define-simple-instruction vm-array-has-fill-pointer-p :pred1 vm-array-has-fill-pointer-p-value)

(define-vm-instruction vm-array-adjustable-p (vm-instruction)
  "Return T if array is adjustable."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :array-adjustable-p) (:sexp-slots dst src))
(define-simple-instruction vm-array-adjustable-p :pred1 vm-adjustable-array-p-value)

(define-vm-instruction vm-adjustable-array-p (vm-instruction)
  "Return T if array is adjustable. ANSI spelling alias."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :adjustable-array-p) (:sexp-slots dst src))
(define-simple-instruction vm-adjustable-array-p :pred1 vm-adjustable-array-p-value)

(define-vm-instruction vm-vector-push (vm-instruction)
  "Push VAL onto ARRAY if below fill-pointer limit. Returns new index or NIL."
  (dst nil :reader vm-dst) (val-reg nil :reader vm-val-reg)
  (array-reg nil :reader vm-array-reg)
  (:sexp-tag :vector-push) (:sexp-slots dst val-reg array-reg))
(defmethod execute-instruction ((inst vm-vector-push) state pc labels)
  (declare (ignore labels))
  (let ((val (vm-reg-get state (vm-val-reg inst)))
        (arr (vm-reg-get state (vm-array-reg inst))))
    (vm-reg-set state (vm-dst inst) (vector-push val (%vm-cow-vector-ensure-writable arr)))
    (values (1+ pc) nil nil)))

(define-vm-instruction vm-vector-pop (vm-instruction)
  "Pop last element from ARRAY (decrement fill pointer). Returns element."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :vector-pop) (:sexp-slots dst src))
(define-simple-instruction vm-vector-pop :unary (lambda (array) (vector-pop (%vm-cow-vector-ensure-writable array))))

(define-vm-instruction vm-set-fill-pointer (vm-instruction)
  "Set fill pointer of ARRAY to NEW-FP. Returns NEW-FP."
  (dst nil :reader vm-dst) (array-reg nil :reader vm-array-reg)
  (val-reg nil :reader vm-val-reg)
  (:sexp-tag :set-fill-pointer) (:sexp-slots dst array-reg val-reg))
(defmethod execute-instruction ((inst vm-set-fill-pointer) state pc labels)
  (declare (ignore labels))
  (let* ((arr (vm-reg-get state (vm-array-reg inst)))
         (fp (vm-reg-get state (vm-val-reg inst))))
    (setf (fill-pointer (%vm-cow-vector-ensure-writable arr)) fp)
    (vm-reg-set state (vm-dst inst) fp)
    (values (1+ pc) nil nil)))

;;; Bit array operations (FR-606), adjust-array/displacement (FR-605),
;;; and simple-vector-p (FR-648) are in array-bits.lisp (loaded next).
