(in-package :cl-cc/vm)

;;; Effective slot computation (across a class precedence list) and the
;;; allocate-instance protocol: raw slot read/write/boundp/makunbound and
;;; sealed-superclass validation. Built on vm-clos-slots-storage.lisp and
;;; vm-clos-slots-introspection.lisp (both load before this file).

(defun %vm-cpl-class-tables (class-ht state)
  "Return CLASS-HT followed by its CPL class tables, most-specific first."
  (let ((cpl (and (hash-table-p class-ht) (gethash :__cpl__ class-ht))))
    (if cpl (loop for class-name in cpl
            for cpl-class = (gethash class-name (vm-class-registry state))
            when cpl-class
              collect cpl-class)
      (and (hash-table-p class-ht) (list class-ht)))))

(defun %vm-first-slot-metadata (classes slot-name key &optional default)
  "Return the first SLOT-NAME metadata value for KEY across CLASSES."
  (dolist (class classes default)
    (when (%vm-class-direct-slot-p class slot-name)
      (let ((entry (assoc slot-name (gethash key class))))
        (when entry
          (return (cdr entry)))))))

(defun %vm-effective-slot-location (class-ht slot-name)
  "Return SLOT-NAME's zero-based effective slot location in CLASS-HT, or NIL."
  (cdr
    (assoc
      slot-name
      (and (hash-table-p class-ht) (gethash :__slot-locations__ class-ht))
      :test
      #'eq)))

(defun %vm-compute-effective-slot-definition (class-ht slot-name state)
  "Build effective slot-definition metadata for SLOT-NAME in CLASS-HT."
  (unless (hash-table-p class-ht)
    (error "compute-effective-slot-definition: ~S is not a class" class-ht))
  (unless (member slot-name (gethash :__slots__ class-ht) :test #'eq)
    (error
      "compute-effective-slot-definition: class ~S has no slot ~S"
      (gethash :__name__ class-ht)
      slot-name))
  (let* ((classes
        (remove-if-not
          (lambda (class)
            (%vm-class-direct-slot-p class slot-name))
          (%vm-cpl-class-tables class-ht state)))
         (slot (make-hash-table :test #'eq))
         (initform-present-p nil)
         (initform (%vm-first-slot-metadata classes slot-name :__initforms__ nil))
         (type (%vm-first-slot-metadata classes slot-name :__slot-types__ t))
         (allocation :instance)
         (initargs nil))
    (dolist (class classes)
      (when (%vm-class-direct-slot-p class slot-name)
        (setf allocation (if (member slot-name (gethash :__class-slots__ class) :test #'eq) :class
            :instance))
        (return)))
    (dolist (class classes)
      (dolist (initarg (%vm-class-slot-initargs-for-slot class slot-name))
        (pushnew initarg initargs :test #'eq)))
    (dolist (class classes)
      (when (and
          (not initform-present-p)
          (%vm-class-direct-slot-p class slot-name)
          (assoc slot-name (gethash :__initforms__ class)))
        (setf initform-present-p t)))
    (setf (gethash :name slot) slot-name
          (gethash :initargs slot) (nreverse initargs)
          (gethash :type slot) (or type t)
          (gethash :allocation slot) allocation
          (gethash :initfunction slot) (let ((value initform))
        (lambda ()
          value))
          (gethash :readers slot) nil
          (gethash :writers slot) nil
          (gethash :location slot) (%vm-effective-slot-location class-ht slot-name))
    (when initform-present-p
      (setf (gethash :initform slot) initform))
    slot))

(defun %vm-raw-allocate-instance (class-ht initarg-regs state)
  "Allocate and initialize raw storage for CLASS-HT without protocol dispatch."
  (let* ((slot-names (gethash :__slots__ class-ht))
         (standard-layout-p (not (%vm-class-nonstandard-metaclass-p class-ht state)))
         (obj-ht
        (if standard-layout-p (allocate-instance-vector class-ht *unbound-slot-marker*)
          (make-hash-table :test (function eq))))
         (initarg-map (gethash :__initargs__ class-ht))
         (initform-values (gethash :__initforms__ class-ht))
         (default-initargs (gethash :__default-initargs__ class-ht))
         (class-slots (gethash :__class-slots__ class-ht))
         (provided-keys
        (remove
          :allow-other-keys
          (mapcar (function car) initarg-regs)
          :test
          (function eq))))
    (unless standard-layout-p
      (setf (gethash :__class__ obj-ht) class-ht))
    (%vm-validate-initargs initarg-regs initarg-map state)
    (dolist (slot-name slot-names)
      (unless (member slot-name class-slots :test (function eq))
        (let ((initform-entry (assoc slot-name initform-values)))
          (%vm-raw-slot-write
            class-ht
            class-slots
            obj-ht
            slot-name
            (if initform-entry (cdr initform-entry)
              nil)))))
    (loop for (initarg-key . default-value) in default-initargs
          unless (member initarg-key provided-keys)
            do (%vm-apply-initarg
        initarg-key
        default-value
        initarg-map
        class-slots
        class-ht
        obj-ht))
    (loop for (initarg-key . value-reg) in initarg-regs
          do (%vm-apply-initarg
        initarg-key
        (vm-reg-get state value-reg)
        initarg-map
        class-slots
        class-ht
        obj-ht))
    obj-ht))

(defun %vm-initarg-values (initarg-regs state)
  "Return INITARG-REGS as alternating key/value arguments."
  (loop for (initarg-key . value-reg) in initarg-regs
        append (list initarg-key (vm-reg-get state value-reg))))

(defun %vm-call-allocation-protocol (class-ht initarg-regs state)
  "Allocate CLASS-HT via custom metaclass hooks when present, otherwise raw allocate."
  (let* ((metaclass (%vm-class-effective-metaclass class-ht state))
         (metaclass-name (and (hash-table-p metaclass) (gethash :__name__ metaclass)))
         (initarg-values (%vm-initarg-values initarg-regs state))
         (allocate-gf (%vm-global-generic-function state 'allocate-instance))
         (initialize-gf (%vm-global-generic-function state 'initialize-instance))
         (obj-ht (if (and allocate-gf
                          metaclass-name
                          (%vm-direct-primary-method-p allocate-gf metaclass-name))
                     (%vm-call-generic-sync allocate-gf state
                                           (append (list class-ht) initarg-values))
                     (%vm-raw-allocate-instance class-ht initarg-regs state))))
    (when initialize-gf
      ;; Dispatch on the instance alone. Method lookup keys on the class of every
      ;; argument, so appending the initargs made the key (class-of obj) plus one
      ;; class per initarg — and an INITIALIZE-INSTANCE method, which by ANSI
      ;; specialises only its first parameter, was registered under a one-element
      ;; key. It could therefore never be found once any initarg was supplied:
      ;; (make-instance 'c) ran the method and (make-instance 'c :x 1) did not.
      ;; The initargs have already been applied to the slots by
      ;; %VM-RAW-ALLOCATE-INSTANCE, so nothing needs them here.
      (%vm-call-generic-sync initialize-gf state
                             (list obj-ht)
                             :default (lambda () obj-ht)))
    obj-ht))

(defun %vm-raw-slot-read (class-ht class-slots obj-ht slot-name)
  "Read SLOT-NAME directly from OBJ-HT/CLASS-HT with standard error behavior."
  (if (and class-slots (member slot-name class-slots :test (function eq))) (%vm-hashlike-gethash slot-name class-ht)
    (let ((all-slots
          (when class-ht
            (%vm-hashlike-gethash :__slots__ class-ht)))
          (class-name
          (when class-ht
            (%vm-hashlike-gethash :__name__ class-ht))))
      (cond
        ((%vm-vector-instance-p obj-ht)
          (let ((index (%vm-slot-vector-index class-ht slot-name)))
            (cond
              ((and index (<= index (%vm-instance-slot-capacity obj-ht)))
                (let ((value (%vm-instance-slot-ref obj-ht index)))
                  (if (eq value *unbound-slot-marker*) (error (make-condition (quote unbound-slot) :name slot-name :instance obj-ht))
                    value)))
              ((and all-slots (member slot-name all-slots :test (function eq)))
                (error (make-condition (quote unbound-slot) :name slot-name :instance obj-ht)))
              (t
                (error
                  "The slot ~S is missing from the object~@[ of class ~S~]"
                  slot-name
                  class-name)))))
        (t
          (let ((obj-storage (%vm-hashlike-storage obj-ht)))
            (unless obj-storage
              (error "The slot ~S is missing from non-object ~S" slot-name obj-ht))
            (multiple-value-bind (value found-p) (gethash slot-name obj-storage)
              (if found-p value
                (let ((str-key (string-downcase (symbol-name slot-name))))
                  (multiple-value-bind (str-val str-found-p) (gethash str-key obj-storage)
                    (if str-found-p str-val
                      (if (and all-slots (member slot-name all-slots :test (function eq))) (error (make-condition (quote unbound-slot) :name slot-name :instance obj-ht))
                        nil))))))))))))

(defun %vm-raw-slot-write (class-ht class-slots obj-ht slot-name value)
  "Write VALUE directly to SLOT-NAME in OBJ-HT/CLASS-HT."
  (if (and class-slots (member slot-name class-slots :test (function eq))) (%vm-hashlike-sethash slot-name class-ht value)
    (if (%vm-vector-instance-p obj-ht) (let ((index (%vm-slot-vector-index class-ht slot-name)))
        (unless (and index (<= index (%vm-instance-slot-capacity obj-ht)))
          (error
            "The slot ~S is missing from the object~@[ of class ~S~]"
            slot-name
            (and class-ht (%vm-hashlike-gethash :__name__ class-ht))))
        (setf (%vm-instance-slot-ref obj-ht index) value))
      (let ((obj-storage (%vm-hashlike-storage obj-ht)))
        (unless obj-storage
          (error "The slot ~S is missing from non-object ~S" slot-name obj-ht))
        (multiple-value-bind (_ sym-found-p) (gethash slot-name obj-storage)
          (declare (ignore _))
          (if sym-found-p (%vm-hashlike-sethash slot-name obj-ht value)
            (let ((str-key (string-downcase (symbol-name slot-name))))
              (multiple-value-bind (_ str-found-p) (gethash str-key obj-storage)
                (declare (ignore _))
                (if str-found-p (%vm-hashlike-sethash str-key obj-ht value)
                  (%vm-hashlike-sethash slot-name obj-ht value))))))))))

(defun %vm-raw-slot-boundp (class-ht class-slots obj-ht slot-name)
  "Return whether SLOT-NAME is bound using direct OBJ-HT/CLASS-HT storage."
  (let ((all-slots
        (when class-ht
          (gethash :__slots__ class-ht))))
    (cond
      ((and all-slots (not (member slot-name all-slots :test (function eq)))) nil)
      ((and class-slots (member slot-name class-slots :test (function eq)))
        (multiple-value-bind (value found-p) (gethash slot-name class-ht)
          (declare (ignore value))
          (if found-p t
            nil)))
      ((%vm-vector-instance-p obj-ht)
        (let ((index (%vm-slot-vector-index class-ht slot-name)))
          (and
            index
            (<= index (%vm-instance-slot-capacity obj-ht))
            (not (eq (%vm-instance-slot-ref obj-ht index) *unbound-slot-marker*)))))
      (t
        (multiple-value-bind (value found-p) (gethash slot-name obj-ht)
          (declare (ignore value))
          (if found-p t
            nil))))))

(defun %vm-raw-slot-makunbound (class-ht class-slots obj-ht slot-name)
  "Make SLOT-NAME unbound using direct OBJ-HT/CLASS-HT storage."
  (if (and class-slots (member slot-name class-slots :test (function eq))) (remhash slot-name class-ht)
    (if (%vm-vector-instance-p obj-ht) (let ((index (%vm-slot-vector-index class-ht slot-name)))
        (unless (and index (<= index (%vm-instance-slot-capacity obj-ht)))
          (error
            "The slot ~S is missing from the object~@[ of class ~S~]"
            slot-name
            (and class-ht (%vm-hashlike-gethash :__name__ class-ht))))
        (setf (%vm-instance-slot-ref obj-ht index) *unbound-slot-marker*))
      (remhash slot-name obj-ht)))
  obj-ht)

(defun %vm-sealed-superclass-name (superclasses registry)
  "Return the first sealed superclass designator in SUPERCLASSES, or NIL."
  (loop for superclass in superclasses
        for class-ht = (if (hash-table-p superclass) superclass
      (gethash superclass registry))
        when (and (hash-table-p class-ht) (gethash :__sealed__ class-ht))
          return (or (gethash :__name__ class-ht) superclass)))
