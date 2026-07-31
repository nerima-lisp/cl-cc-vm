(in-package :cl-cc/vm)

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — CLOS Instruction Execution Methods
;;;
;;; Contains all execute-instruction methods for CLOS VM instructions:
;;; vm-class-def (class registration + CPL), vm-make-obj (instance creation
;;; with initarg validation), vm-slot-read/write, vm-slot-boundp/makunbound/
;;; exists-p, vm-register-method, vm-generic-call; plus FR-677 class-name/
;;; class-of/typep instructions and change-class.
;;;
;;; Instruction defstructs (define-vm-instruction forms) and MRO helpers
;;; (collect-inherited-slots, compute-class-precedence-list) are in
;;; vm-clos.lisp; slot/class metaobject helpers are in vm-clos-slots.lisp
;;; (both load before).
;;;
;;; Load order: after vm-clos-slots.lisp, before vm-dispatch-gf.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
(defmethod execute-instruction ((inst vm-class-def) state pc labels)
  (declare (ignore labels))
  (let* ((class-ht (make-hash-table :test #'eq))
         (registry (vm-class-registry state))
         (supers (vm-superclasses inst))
         (all-slots (%vm-cdef-collect-slots supers (vm-slot-names inst) registry))
         (all-initargs
        (%vm-cdef-collect-initargs supers (vm-slot-initargs inst) registry))
         (all-default-initargs
        (%vm-cdef-collect-default-initargs inst supers registry state))
         (all-slot-types (%vm-cdef-collect-slot-types inst supers registry))
         (all-class-slots (%vm-cdef-collect-class-slots inst supers registry))
         (all-slot-locations (%vm-cdef-slot-locations all-slots))
         (previous-class (gethash (vm-class-name-sym inst) registry))
         (own-initform-values
        (loop for (slot-name . reg) in (vm-slot-initform-regs inst)
              collect (cons slot-name (vm-reg-get state reg))))
         (all-initform-values
        (%vm-cdef-collect-initforms supers own-initform-values registry)))
    (when (and (hash-table-p previous-class) (gethash :__sealed__ previous-class))
      (error "Cannot redefine sealed class ~S" (vm-class-name-sym inst)))
    (let ((sealed-superclass (%vm-sealed-superclass-name supers registry)))
      (when sealed-superclass
        (error "Cannot subclass sealed class ~S" sealed-superclass)))
    (setf (gethash :__name__ class-ht) (vm-class-name-sym inst)
          (gethash :__superclasses__ class-ht) supers
          (gethash :__direct-slots__ class-ht) (vm-slot-names inst)
          (gethash :__slots__ class-ht) all-slots
          (gethash :__initargs__ class-ht) all-initargs
          (gethash :__methods__ class-ht) (make-hash-table :test #'equal)
          (gethash :__eql-index__ class-ht) (make-hash-table :test #'equal)
          (gethash :__satiated__ class-ht) nil
          (gethash :__ic-sites__ class-ht) nil
          (gethash :__initforms__ class-ht) all-initform-values
          (gethash :__default-initargs__ class-ht) all-default-initargs
          (gethash :__slot-types__ class-ht) all-slot-types
          (gethash :__class-slots__ class-ht) all-class-slots
          (gethash :__slot-locations__ class-ht) all-slot-locations
          (gethash :__previous-class__ class-ht) previous-class
          (gethash :__sealed__ class-ht) (if (vm-sealed-p inst) t
        nil)
          (gethash :__metaclass__ class-ht) (if (vm-metaclass-reg inst) (vm-reg-get state (vm-metaclass-reg inst))
        'standard-class))
    (let ((metaclass-class
          (%vm-resolve-class-designator (gethash :__metaclass__ class-ht) state)))
      (when (and metaclass-class (not (%vm-standard-metaclass-p metaclass-class)))
        (setf (gethash :__class__ class-ht) metaclass-class)))
    (%vm-mark-class-obsolete previous-class class-ht)
    (%vm-cdef-init-class-slots class-ht all-class-slots all-initform-values)
    (setf (gethash (vm-class-name-sym inst) registry) class-ht
          (gethash :__cpl__ class-ht) (compute-class-precedence-list (vm-class-name-sym inst) registry))
    (vm-reg-set state (vm-dst inst) class-ht)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-make-obj) state pc labels)
  (declare (ignore labels))
  (let* ((class-ht (vm-reg-get state (vm-class-reg inst)))
         ;; The initialization protocol also has to run for a *standard* class
         ;; once the program defines INITIALIZE-INSTANCE methods: ANSI calls
         ;; INITIALIZE-INSTANCE on every new instance, not only on those of a
         ;; custom metaclass. Gating on the generic function's existence — one
         ;; GETHASH — keeps the raw fast path for the overwhelmingly common case
         ;; of a program that defines none.
         (obj-ht (if (or (%vm-class-nonstandard-metaclass-p class-ht state)
                         (%vm-global-generic-function state 'initialize-instance))
                     (%vm-call-allocation-protocol class-ht (vm-initarg-regs inst) state)
                     (%vm-raw-allocate-instance class-ht (vm-initarg-regs inst) state))))
    (vm-reg-set state (vm-dst inst) obj-ht)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-slot-read) state pc labels)
  (declare (ignore labels))
  (let* ((obj-ht (vm-reg-get state (vm-obj-reg inst)))
         (slot-name (vm-slot-name-sym inst)))
    (multiple-value-bind (class-ht class-slots) (%vm-class-slots-of obj-ht)
      (let ((raw-reader
            (lambda ()
              (%vm-raw-slot-read class-ht class-slots obj-ht slot-name))))
        (vm-reg-set
          state
          (vm-dst inst)
          (if (and class-ht (%vm-class-nonstandard-metaclass-p class-ht state)) (let ((gf (%vm-global-generic-function state 'slot-value-using-class)))
              (if gf (%vm-call-generic-sync
                  gf
                  state
                  (list class-ht obj-ht slot-name)
                  :default
                  raw-reader)
                (funcall raw-reader)))
            (funcall raw-reader)))))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-slot-write) state pc labels)
  (declare (ignore labels))
  (let* ((obj-ht (vm-reg-get state (vm-obj-reg inst)))
         (slot-name (vm-slot-name-sym inst))
         (value (vm-reg-get state (vm-value-reg inst))))
    (multiple-value-bind (class-ht class-slots) (%vm-class-slots-of obj-ht)
      (let ((raw-writer
            (lambda ()
              (%vm-raw-slot-write class-ht class-slots obj-ht slot-name value))))
        (if (and class-ht (%vm-class-nonstandard-metaclass-p class-ht state)) (let ((gf
                (%vm-global-generic-function
                  state
                  '(setf slot-value-using-class))))
            (if gf (%vm-call-generic-sync
                gf
                state
                (list value class-ht obj-ht slot-name)
                :default
                raw-writer)
              (funcall raw-writer)))
          (funcall raw-writer))))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-slot-boundp) state pc labels)
  (declare (ignore labels))
  (let* ((obj-ht (vm-reg-get state (vm-obj-reg inst)))
         (slot-name (vm-slot-name-sym inst)))
    (multiple-value-bind (class-ht class-slots) (%vm-class-slots-of obj-ht)
      (let ((raw-boundp
            (lambda ()
              (%vm-raw-slot-boundp class-ht class-slots obj-ht slot-name))))
        (vm-reg-set
          state
          (vm-dst inst)
          (if (and class-ht (%vm-class-nonstandard-metaclass-p class-ht state)) (let ((gf (%vm-global-generic-function state 'slot-boundp-using-class)))
              (if gf (%vm-call-generic-sync
                  gf
                  state
                  (list class-ht obj-ht slot-name)
                  :default
                  raw-boundp)
                (funcall raw-boundp)))
            (funcall raw-boundp)))))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-slot-makunbound) state pc labels)
  (declare (ignore labels))
  (let* ((obj-ht (vm-reg-get state (vm-obj-reg inst)))
         (slot-name (vm-slot-name-sym inst)))
    (multiple-value-bind (class-ht class-slots) (%vm-class-slots-of obj-ht)
      (let ((raw-makunbound
            (lambda ()
              (%vm-raw-slot-makunbound class-ht class-slots obj-ht slot-name))))
        (vm-reg-set
          state
          (vm-dst inst)
          (if (and class-ht (%vm-class-nonstandard-metaclass-p class-ht state)) (let ((gf (%vm-global-generic-function state 'slot-makunbound-using-class)))
              (if gf (%vm-call-generic-sync
                  gf
                  state
                  (list class-ht obj-ht slot-name)
                  :default
                  raw-makunbound)
                (funcall raw-makunbound)))
            (funcall raw-makunbound)))))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-slot-exists-p) state pc labels)
  (declare (ignore labels))
  (let* ((obj-ht (vm-reg-get state (vm-obj-reg inst)))
         (slot-name (vm-slot-name-sym inst))
         (class-ht (%vm-obj-class-ht obj-ht))
         (slots
        (when class-ht
          (gethash :__slots__ class-ht))))
    (vm-reg-set
      state
      (vm-dst inst)
      (if (and slots (member slot-name slots)) t
        nil))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-compute-effective-slot-definition) state pc labels)
  (declare (ignore labels))
  (let ((class (vm-reg-get state (vm-lhs inst)))
        (slot-name (vm-reg-get state (vm-rhs inst))))
    (vm-reg-set
      state
      (vm-dst inst)
      (%vm-compute-effective-slot-definition class slot-name state))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-register-method) state pc labels)
  (declare (ignore labels))
  (let* ((gf-ht (vm-reg-get state (vm-gf-reg inst)))
         (methods-ht (when (and (hash-table-p gf-ht)
                                (nth-value 1 (gethash :__methods__ gf-ht)))
                       (gethash :__methods__ gf-ht)))
         (eql-index (when (hash-table-p gf-ht) (gethash :__eql-index__ gf-ht)))
         (specializer (vm-method-specializer inst))
         (qualifier (vm-method-qualifier inst))
         (method-closure (vm-reg-get state (vm-method-reg inst))))
    (when methods-ht
      ;; Adding a method after a GF was marked closed for optimization is
      ;; allowed, but it re-opens dispatch and invalidates existing ICs.
      (%mop-invalidate-gf gf-ht)
      ;; Create method descriptor with qualifier metadata
      (let ((desc (make-hash-table :test 'eq)))
        (setf (gethash :function desc) method-closure
              (gethash :qualifiers desc) (if (null qualifier) nil (list qualifier))
              (gethash :specializer desc) specializer
              (gethash :gf desc) gf-ht)
        (if (null qualifier)
            (progn
              (setf (gethash specializer methods-ht) desc)
              (when eql-index
                (dolist (eql-key (%vm-extract-eql-specializer-keys specializer))
                  (pushnew method-closure (gethash eql-key eql-index) :test #'eq))))
            (let ((qual-key (intern (format nil "__~A__" (string-upcase (string qualifier)))
                                    :keyword)))
              (unless (gethash qual-key gf-ht)
                (setf (gethash qual-key gf-ht) (make-hash-table :test 'equal)))
              (let ((qual-ht (gethash qual-key gf-ht)))
                (if (eq qualifier :around)
                    (setf (gethash specializer qual-ht) desc)
                    (push desc (gethash specializer qual-ht))))))))
    ;; FR-009: Method registration invalidates monomorphic IC entries.
    (%ic-clear-gf-caches gf-ht)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-generic-call) state pc labels)
  (let* ((gf-ht (vm-reg-get state (vm-gf-reg inst)))
         (arg-regs (vm-args inst))
         (dst-reg (vm-dst inst)))
    (%ic-lookup-or-dispatch gf-ht state pc arg-regs dst-reg labels inst)))

;;; FR-677: class-name and class-of
(define-vm-unary-instruction
  vm-class-name-fn
  :class-name
  "Return the name (symbol) of a class object.")

(defmethod execute-instruction ((inst vm-class-name-fn) state pc labels)
  (declare (ignore labels))
  (let ((class (vm-reg-get state (vm-src inst))))
    (vm-reg-set
      state
      (vm-dst inst)
      (if (%vm-hashlike-storage class) (or
          (%vm-hashlike-gethash :__name__ class)
          (error "class-name: not a class object"))
        (error "class-name: ~A is not a class" class)))
    (values (1+ pc) nil nil)))

(define-vm-unary-instruction
  vm-class-of-fn
  :class-of
  "Return the class object of an instance.")

(defmethod execute-instruction ((inst vm-class-of-fn) state pc labels)
  (declare (ignore labels))
  (let ((obj (vm-reg-get state (vm-src inst))))
    (vm-reg-set
      state
      (vm-dst inst)
      (let ((class (%vm-obj-class-ht obj)))
        (if class (if (%vm-class-nonstandard-metaclass-p class state) (%vm-class-effective-metaclass class state)
            class)
          (error "class-of: ~A has no class" obj))))
    (values (1+ pc) nil nil)))

;;; FR-552: find-class / (setf find-class)
(define-vm-unary-instruction
  vm-find-class
  :find-class
  "Look up a class by name in the class registry.")

(defmethod execute-instruction ((inst vm-find-class) state pc labels)
  (declare (ignore labels))
  (let* ((name (vm-reg-get state (vm-src inst)))
         (class (gethash name (vm-class-registry state))))
    (unless class
      ;; Also try global vars as fallback
      (setf class (gethash name (vm-global-vars state))))
    (vm-reg-set state (vm-dst inst) (or class nil))
    (values (1+ pc) nil nil)))
