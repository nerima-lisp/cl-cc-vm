(in-package :cl-cc/vm)

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — CLOS Slot Collection and Instance Storage
;;;
;;; Slot/initarg/initform/default-initarg/slot-type collection across a
;;; class precedence list, and vector- and hash-table-backed instance
;;; storage (the hashlike/vector-instance layer obsolescence updates and
;;; raw slot access read through).
;;;
;;; Instruction defstructs (define-vm-instruction forms) and MRO helpers
;;; (collect-inherited-slots, compute-class-precedence-list) are in
;;; vm-clos.lisp (loads before). Class/generic-function introspection is
;;; in vm-clos-slots-introspection.lisp; effective-slot computation and
;;; the allocate-instance protocol are in vm-clos-slots.lisp (both load
;;; after this file).
;;;
;;; Load order: after vm-clos.lisp, before vm-clos-slots-introspection.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
(defparameter *unbound-slot-marker* (gensym "UNBOUND-SLOT-")
  "Unique sentinel stored in vector-backed instances for unbound slots.")

(defun %vm-cdef-collect-slots (supers own-slots registry)
  "Merge inherited and own slot names; inherited slots come first."
  (let ((inherited (collect-inherited-slots supers registry)))
    (append
      inherited
      (remove-if
        (lambda (s)
          (member s inherited))
        own-slots))))

(defun %vm-cdef-collect-initargs (supers own-initargs registry)
  "Merge inherited and own initarg→slot alist; inherited entries come first."
  (let ((inherited (collect-inherited-initargs supers registry)))
    (append
      inherited
      (remove-if
        (lambda (e)
          (assoc (car e) inherited :test #'eq))
        own-initargs))))

(defun %vm-cdef-collect-initforms (supers own-initforms registry)
  "Merge slot initforms according to effective-slot precedence.
OWN-INITFORMS wins over inherited initforms.  Among inherited slots, earlier
superclasses are more specific than later superclasses, and each superclass
descriptor already carries its effective inherited initforms."
  (let ((inherited nil))
    (dolist (super supers)
      (let ((super-ht (gethash super registry)))
        (when super-ht
          (dolist (entry (gethash :__initforms__ super-ht))
            (unless (or
                (assoc (car entry) own-initforms :test #'eq)
                (assoc (car entry) inherited :test #'eq))
              (push entry inherited))))))
    (append own-initforms (nreverse inherited))))

(defun %vm-cdef-collect-default-initargs (inst supers registry state)
  "Collect default-initarg key→value pairs; own values take precedence over inherited."
  (let ((own
        (loop for (key . reg) in (vm-default-initarg-regs inst)
              collect (cons key (vm-reg-get state reg))))
        (inherited
        (loop for super in supers
              for super-ht = (gethash super registry)
              when super-ht
                append (gethash :__default-initargs__ super-ht))))
    (append
      own
      (remove-if
        (lambda (e)
          (assoc (car e) own))
        inherited))))

(defun %vm-cdef-collect-slot-types (inst supers registry)
  "Collect slot-name→type pairs; own slot type declarations take precedence."
  (let ((own (vm-slot-types inst))
        (inherited
        (loop for super in supers
              for super-ht = (gethash super registry)
              when super-ht
                append (gethash :__slot-types__ super-ht))))
    (append
      own
      (remove-if
        (lambda (e)
          (assoc (car e) own))
        inherited))))

(defun %vm-cdef-collect-class-slots (inst supers registry)
  "Union of own class-allocated slots with all inherited class-allocated slots."
  (let ((inherited
        (loop for super in supers
              for super-ht = (gethash super registry)
              when super-ht
                append (gethash :__class-slots__ super-ht))))
    (union (vm-class-slots inst) inherited :test #'eq)))

(defun %vm-cdef-slot-locations (slots)
  "Return SLOT-NAME→zero-based-index metadata for the effective slot list SLOTS."
  (loop for slot in slots
        for index from 0
        collect (cons slot index)))

(defun %vm-cdef-init-class-slots (class-ht all-class-slots initform-values)
  "Initialize class-allocated slots on CLASS-HT from INITFORM-VALUES (skip existing values)."
  (dolist (slot-name all-class-slots)
    (unless (gethash slot-name class-ht)
      (let ((entry (assoc slot-name initform-values)))
        (setf (gethash slot-name class-ht) (if entry (cdr entry)
            nil))))))

(defun %vm-mark-class-obsolete (old-class replacement-class)
  "Record that OLD-CLASS should lazily migrate instances to REPLACEMENT-CLASS."
  (when (hash-table-p old-class)
    (setf (gethash :__obsolete__ old-class) t
          (gethash :__replacement-class__ old-class) replacement-class))
  old-class)

(defun %vm-hashlike-storage (table)
  "Return TABLE's host hash-table storage for host and VM hash tables."
  (cond
    ((hash-table-p table) table)
    ((typep table 'vm-hash-table-object) (vm-hash-table-internal table))
    (t nil)))

(defun %vm-hashlike-gethash (key table &optional default)
  "GETHASH for host hash tables and VM hash-table objects."
  (let ((storage (%vm-hashlike-storage table)))
    (if storage (gethash key storage default)
      default)))

(defun %vm-hashlike-sethash (key table value)
  "Set KEY in a host hash table or VM hash-table object."
  (let ((storage (%vm-hashlike-storage table)))
    (unless storage
      (error "Expected hash table, got ~S" table))
    (setf (gethash key storage) value)))

(defun %vm-vector-instance-p (object)
  "Return T when OBJECT is a vector-backed standard instance."
  (and (vectorp object) (plusp (length object)) (hash-table-p (aref object 0))))

(defun %vm-instance-slot-capacity (instance)
  (let ((storage (and (> (length instance) 1) (aref instance 1))))
    (if (%vm-instance-storage-p storage) (length (%vm-instance-storage-values storage))
      (1- (length instance)))))

(defun %vm-replace-instance-slot-storage (instance values)
  (let ((storage (and (> (length instance) 1) (aref instance 1))))
    (unless (%vm-instance-storage-p storage)
      (error "Cannot grow legacy direct-layout instance ~S" instance))
    (setf (%vm-instance-storage-values storage) values)))

(defun %vm-instance-slot-ref (instance index)
  (slot-value-by-index instance index))

(defun (setf %vm-instance-slot-ref) (value instance index)
  (setf (slot-value-by-index instance index) value))

(defun %vm-slot-vector-index (class-ht slot-name)
  "Return SLOT-NAME's vector index in CLASS-HT instances, or NIL."
  (let ((location (%vm-effective-slot-location class-ht slot-name)))
    (and location (1+ location))))

(defun %vm-update-obsolete-instance (obj-ht)
  "Migrate OBJ-HT from an obsolete class descriptor to its replacement, if any."
  (let* ((old-class
        (cond
          ((hash-table-p obj-ht) (gethash :__class__ obj-ht))
          ((%vm-vector-instance-p obj-ht) (aref obj-ht 0))))
         (new-class
        (and
          (hash-table-p old-class)
          (gethash :__obsolete__ old-class)
          (gethash :__replacement-class__ old-class))))
    (when (hash-table-p new-class)
      (let ((old-slots (gethash :__slots__ old-class))
            (new-slots (gethash :__slots__ new-class))
            (new-initforms (gethash :__initforms__ new-class)))
        (cond
          ((hash-table-p obj-ht)
            (dolist (slot old-slots)
              (unless (member slot new-slots :test (function eq))
                (remhash slot obj-ht)))
            (dolist (slot new-slots)
              (multiple-value-bind (value found-p) (gethash slot obj-ht)
                (declare (ignore value))
                (unless found-p
                  (let ((entry (assoc slot new-initforms :test (function eq))))
                    (setf (gethash slot obj-ht) (if entry (cdr entry)
                        nil))))))
            (setf (gethash :__class__ obj-ht) new-class))
          ((%vm-vector-instance-p obj-ht)
            (let ((old-values nil))
              (dolist (slot old-slots)
                (let ((index (%vm-slot-vector-index old-class slot)))
                  (when (and index (<= index (%vm-instance-slot-capacity obj-ht)))
                    (push (cons slot (%vm-instance-slot-ref obj-ht index)) old-values))))
              (when (> (length new-slots) (%vm-instance-slot-capacity obj-ht))
                (%vm-replace-instance-slot-storage
                  obj-ht
                  (make-array (length new-slots) :initial-element *unbound-slot-marker*)))
              (loop for index from 1 to (%vm-instance-slot-capacity obj-ht)
                    do (setf (%vm-instance-slot-ref obj-ht index) *unbound-slot-marker*))
              (dolist (slot new-slots)
                (let ((index (%vm-slot-vector-index new-class slot))
                      (old-entry (assoc slot old-values :test (function eq)))
                      (initform-entry (assoc slot new-initforms :test (function eq))))
                  (setf (%vm-instance-slot-ref obj-ht index) (cond
                      (old-entry (cdr old-entry))
                      (initform-entry (cdr initform-entry))
                      (t nil)))))
              (setf (aref obj-ht 0) new-class)))))
      obj-ht)))
