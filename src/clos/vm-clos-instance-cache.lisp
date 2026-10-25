(in-package :cl-cc/vm)

;;; ── FR-888/FR-889 allocation caches ──────────────────────────────────────

(defvar *vm-next-class-id* 0
  "Monotonic class id source used by vector-backed instances.")

(defun %vm-ensure-class-id (class-ht)
  "Return CLASS-HT's stable numeric class id, assigning one if needed."
  (or (gethash :__class-id__ class-ht)
      (setf (gethash :__class-id__ class-ht) (incf *vm-next-class-id*))))

(defun %vm-build-slot-vector-index (slots)
  "Build a SLOT-NAME→storage-index hash table for vector instance slots.
Index 0 is reserved for the class header, so the first slot starts at 1."
  (let ((index (make-hash-table :test #'eq)))
    (loop for slot-name in slots
          for storage-index from 1
          do (setf (gethash slot-name index) storage-index))
    index))

(defun %vm-fixed-slot-layout-p (class-ht)
  "Return true when CLASS-HT can use vector-backed fixed slot storage."
  (and (hash-table-p class-ht)
       (not (gethash :__dynamic-slot-layout__ class-ht))
       (not (gethash :__obsolete__ class-ht))))

(defun class-slot-vector-index (class-ht slot-name)
  "Return SLOT-NAME's O(1) vector storage index in CLASS-HT, or NIL.
The cache is created during finalization and lazily rebuilt for older class
tables that predate FR-888 metadata."
  (when (hash-table-p class-ht)
    (let ((index (or (gethash :__slot-vector-index__ class-ht)
                     (setf (gethash :__slot-vector-index__ class-ht)
                           (%vm-build-slot-vector-index
                            (gethash :__slots__ class-ht))))))
      (gethash slot-name index))))

(defun allocate-instance-vector (class-ht &optional initial-element)
  "Allocate a vector-backed instance for CLASS-HT."
  (unless (%vm-fixed-slot-layout-p class-ht)
    (error "Class ~S does not have a fixed slot layout"
           (and (hash-table-p class-ht) (gethash :__name__ class-ht))))
  (%vm-ensure-class-id class-ht)
  (vector class-ht
          (%make-vm-instance-storage
           (make-array (length (gethash :__slots__ class-ht))
                       :initial-element initial-element))))

(defun slot-value-by-index (instance index)
  "Return INSTANCE slot value at vector storage INDEX in O(1)."
  (unless (and (vectorp instance) (> (length instance) 1)
               (hash-table-p (aref instance 0)) (integerp index) (plusp index))
    (error "Invalid vector slot index ~S for ~S" index instance))
  (let ((storage (aref instance 1)))
    (if (%vm-instance-storage-p storage)
        (let ((values (%vm-instance-storage-values storage)))
          (unless (< (1- index) (length values))
            (error "Invalid vector slot index ~S for ~S" index instance))
          (aref values (1- index)))
        (progn
          (unless (< index (length instance))
            (error "Invalid vector slot index ~S for ~S" index instance))
          (aref instance index)))))

(defun (setf slot-value-by-index) (value instance index)
  "Set INSTANCE slot value at vector storage INDEX in O(1)."
  (unless (and (vectorp instance) (> (length instance) 1)
               (hash-table-p (aref instance 0)) (integerp index) (plusp index))
    (error "Invalid vector slot index ~S for ~S" index instance))
  (let ((storage (aref instance 1)))
    (if (%vm-instance-storage-p storage)
        (let ((values (%vm-instance-storage-values storage)))
          (unless (< (1- index) (length values))
            (error "Invalid vector slot index ~S for ~S" index instance))
          (setf (aref values (1- index)) value))
        (progn
          (unless (< index (length instance))
            (error "Invalid vector slot index ~S for ~S" index instance))
          (setf (aref instance index) value)))))

(defun %vm-build-instance-template (class-ht &optional initial-element)
  "Build the zero-arg make-instance template for CLASS-HT."
  (allocate-instance-vector class-ht initial-element))

(defun %vm-initargs-signature (initargs)
  "Return a compact cache signature for INITARGS.
Only keys affect the specialized constructor path; values remain per-call."
  (loop for entry in initargs
        for key = (car entry)
        unless (eq key :allow-other-keys)
          collect key))

(defun %vm-make-instance-cache (class-ht)
  "Return CLASS-HT's make-instance specialized path cache."
  (or (gethash :__make-instance-cache__ class-ht)
      (setf (gethash :__make-instance-cache__ class-ht)
            (make-hash-table :test #'equal))))

