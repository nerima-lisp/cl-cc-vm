(in-package :cl-cc/vm)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — MOP Introspection and Method Dispatch
;;;
;;; Contains: FR-930/FR-931 MOP helpers (%mop-*), slot-definition-*,
;;; class-direct-*, generic-function-methods, method-*, add-method,
;;; remove-method, ensure-generic-function, slot-value-using-class.
;;;
;;; FR-888/FR-889 instance allocation caching (%vm-ensure-class-id through
;;; finalize-class-allocation-cache) is in vm-clos-instance-cache.lisp
;;; (loads next).
;;;
;;; Load order: after vm-clos.lisp, before vm-clos-instance-cache.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ── FR-930/FR-931 MOP introspection and method selection ────────────────

(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow '(add-method compute-applicable-methods
            compute-applicable-methods-using-classes ensure-generic-function
            find-method method-qualifiers method-specializers remove-method)
          :cl-cc/vm))

(defvar *mop-generic-function-registry* (make-hash-table :test #'equal)
  "Host-side registry for generic functions created by ENSURE-GENERIC-FUNCTION.")

(progn
  (defvar *unbound-slot-marker*)
  (defstruct (%vm-instance-storage (:constructor %make-vm-instance-storage (values)))
    values))

(defun %mop-hash-table-values (table)
  "Return values from TABLE in an unspecified but stable traversal list."
  (let (values)
    (when (hash-table-p table)
      (maphash (lambda (key value)
                 (declare (ignore key))
                 (push value values))
               table))
    (nreverse values)))

(defun %mop-method-value->list (value)
  "Normalize a method table VALUE to a flat list of methods."
  (cond
    ((null value) nil)
    ((and (listp value) (not (%eql-specializer-p value))) (copy-list value))
    (t (list value))))

(defun %mop-slot-initargs-for-slot (class slot-name)
  "Return initargs in CLASS that initialize SLOT-NAME."
  (let (initargs)
    (dolist (entry (and (hash-table-p class) (gethash :__initargs__ class)))
      (when (eq (cdr entry) slot-name)
        (pushnew (car entry) initargs :test #'eq)))
    (nreverse initargs)))

(defun %mop-slot-definition (class slot-name)
  "Build a lightweight slot-definition descriptor for SLOT-NAME in CLASS."
  (let ((slot (make-hash-table :test #'eq)))
    (setf (gethash :name slot) slot-name
          (gethash :initargs slot) (%mop-slot-initargs-for-slot class slot-name)
          (gethash :type slot) (or (cdr (assoc slot-name (gethash :__slot-types__ class)
                                               :test #'eq))
                                   t)
          (gethash :allocation slot)
          (if (member slot-name (gethash :__class-slots__ class) :test #'eq)
              :class
              :instance)
          (gethash :location slot)
          (cdr (assoc slot-name (gethash :__slot-locations__ class) :test #'eq)))
    (let ((initform (assoc slot-name (gethash :__initforms__ class) :test #'eq)))
      (when initform
        (setf (gethash :initform slot) (cdr initform))))
    slot))

(defun %mop-slot-definitions (class key)
  "Return slot-definition descriptors for slot names stored under KEY."
  (when (hash-table-p class)
    (mapcar (lambda (slot-name) (%mop-slot-definition class slot-name))
            (gethash key class))))

(defun class-slots (class)
  "Return effective slot-definition objects for CLASS."
  (%mop-slot-definitions class :__slots__))

(defun slot-definition-name (slot)
  "Return SLOT's name."
  (cond ((hash-table-p slot) (gethash :name slot))
        ((symbolp slot) slot)
        (t nil)))

(defmacro define-slot-definition-accessor (name key &optional default)
  "Define NAME as a reader of SLOT's KEY metadata, defaulting to DEFAULT
when SLOT is not a hash-table-backed slot-definition descriptor or KEY is
absent."
  `(defun ,name (slot)
     ,(format nil "Return SLOT's ~(~A~) metadata, defaulting to ~S." key default)
     (if (hash-table-p slot) (or (gethash ,key slot) ,default) ,default)))

(define-slot-definition-accessor slot-definition-type :type t)
(define-slot-definition-accessor slot-definition-initform :initform)
(define-slot-definition-accessor slot-definition-initargs :initargs)
(define-slot-definition-accessor slot-definition-allocation :allocation :instance)
(define-slot-definition-accessor slot-definition-location :location)

(defun class-precedence-list (class)
  "Return CLASS's class precedence list."
  (when (hash-table-p class)
    (or (gethash :__cpl__ class)
        (let ((name (gethash :__name__ class)))
          (and name (cons name (copy-list (gethash :__superclasses__ class))))))))

(defun generic-function-methods (gf)
  "Return all method descriptors registered on GF, including qualified methods."
  (let (methods)
    (when (hash-table-p gf)
      (dolist (table-key '(:__methods__ :__BEFORE__ :__AFTER__ :__AROUND__))
        (dolist (value (%mop-hash-table-values (gethash table-key gf)))
          (dolist (method (%mop-method-value->list value))
            (pushnew method methods :test #'eq)))))
    (nreverse methods)))

(defun method-specializers (method)
  "Return METHOD specializers as a list."
  (when (hash-table-p method)
    (or (gethash :specializers method)
        (let ((specializer (gethash :specializer method)))
          (cond ((null specializer) nil)
                ((and (listp specializer) (not (%eql-specializer-p specializer)))
                 specializer)
                (t (list specializer)))))))

(defun method-qualifiers (method)
  "Return METHOD qualifiers."
  (if (hash-table-p method) (or (gethash :qualifiers method) nil) nil))

(defun generic-function-method-combination (gf)
  "Return GF's method-combination object, defaulting to STANDARD."
  (if (and (hash-table-p gf) (nth-value 1 (gethash :__method-combination__ gf)))
      (gethash :__method-combination__ gf)
      'standard))

(progn
  (defvar *satiated-generic-functions* nil
    "VM generic functions explicitly marked as satiated in this host session.")
  (defvar *satiating-gfs-active-p* nil
    "True while WITH-SATIATING-GFS is establishing a satiation session.")
  (defun satiate-generic-function (gf)
    "Mark GF as closed to method addition for dispatch optimization."
    (unless (vm-generic-function-p gf)
      (error "satiate-generic-function: ~S is not a VM generic function" gf))
    (setf (gethash :__satiated__ gf) t)
    (pushnew gf *satiated-generic-functions* :test #'eq)
    gf)
  (defmacro with-satiating-gfs ((&rest generic-functions) &body body)
    "Satiate GENERIC-FUNCTIONS while evaluating BODY."
    `(let ((*satiating-gfs-active-p* t))
       (mapc #'satiate-generic-function (list ,@generic-functions))
       ,@body))
  (defun satiating-gfs-p (&optional (gf nil supplied-p))
    "Report whether GF, or the current VM GF satiation session, is active."
    (if supplied-p
        (and (vm-generic-function-p gf) (gethash :__satiated__ gf) t)
        (or *satiating-gfs-active-p*
            (some (lambda (candidate)
                    (and (vm-generic-function-p candidate)
                         (gethash :__satiated__ candidate)))
                  *satiated-generic-functions*))))
  (vm-register-host-bridge 'satiate-generic-function #'satiate-generic-function)
  (vm-register-host-bridge 'satiating-gfs-p #'satiating-gfs-p))

(defun %mop-normalize-specializer-key (specializers)
  "Return the dispatch-table key for SPECIALIZERS."
  (if (and (listp specializers) (= (length specializers) 1))
      (first specializers)
      specializers))

(defun %mop-qualified-table-key (qualifiers)
  "Return the GF table key for QUALIFIERS."
  (if qualifiers
      (intern (format nil "__~A__" (string-upcase (string (first qualifiers)))) :keyword)
      :__methods__))

(defun %mop-class-cpl (class)
  "Return CLASS's CPL as symbol designators, always ending with T."
  (let ((cpl (cond ((hash-table-p class) (or (gethash :__cpl__ class)
                                             (list (gethash :__name__ class))))
                   ((symbolp class) (list class))
                   (t (list t)))))
    (if (member t cpl :test #'eq) cpl (append cpl (list t)))))

(defun %mop-arg-class (arg)
  "Return ARG's class descriptor/name for class-only MOP dispatch."
  (cond ((and (vectorp arg) (plusp (length arg)) (hash-table-p (aref arg 0)))
         (aref arg 0))
        ((hash-table-p arg) (or (gethash :__class__ arg) 'hash-table))
        ((integerp arg) 'integer)
        ((stringp arg) 'string)
        ((symbolp arg) 'symbol)
        (t t)))

(defun %mop-specializer-matches-arg-p (specializer arg)
  "Return true when SPECIALIZER accepts ARG."
  (cond ((eq specializer t) t)
        ((%eql-specializer-p specializer) (eql (second specializer) arg))
        (t (member specializer (%mop-class-cpl (%mop-arg-class arg)) :test #'eq))))

(defun %mop-method-applicable-to-args-p (method args)
  "Return true when METHOD is applicable to ARGS."
  (let ((specializers (method-specializers method)))
    (and (= (length specializers) (length args))
         (every #'%mop-specializer-matches-arg-p specializers args))))

(defun %mop-method-applicable-to-classes-p (method classes)
  "Return true when METHOD is applicable to argument CLASSES."
  (let ((specializers (method-specializers method)))
    (and (= (length specializers) (length classes))
         (every (lambda (specializer class)
                  (and (not (%eql-specializer-p specializer))
                       (or (eq specializer t)
                           (member specializer (%mop-class-cpl class) :test #'eq))))
                specializers classes))))

(defun compute-applicable-methods (gf args)
  "Return methods of GF applicable to ARGS."
  (remove-if-not (lambda (method) (%mop-method-applicable-to-args-p method args))
                 (generic-function-methods gf)))

(defun compute-applicable-methods-using-classes (gf classes)
  "Return (values METHODS DEFINITIVEP) for GF and argument CLASSES."
  (let* ((methods (generic-function-methods gf))
         (has-eql-p (some (lambda (method)
                            (some #'%eql-specializer-p (method-specializers method)))
                          methods)))
    (values (remove-if-not (lambda (method)
                             (%mop-method-applicable-to-classes-p method classes))
                           methods)
            (not has-eql-p))))

(defun find-method (gf qualifiers specializers &optional (errorp t))
  "Find the method on GF with QUALIFIERS and SPECIALIZERS."
  (let* ((table (and (hash-table-p gf) (gethash (%mop-qualified-table-key qualifiers) gf)))
         (key (%mop-normalize-specializer-key specializers))
         (candidates (%mop-method-value->list (and table (gethash key table))))
         (method (find-if (lambda (method)
                            (and (equal (method-qualifiers method) qualifiers)
                                 (equal (method-specializers method) specializers)))
                          candidates)))
    (cond (method method)
          (errorp (error "No method on ~S with qualifiers ~S and specializers ~S"
                         gf qualifiers specializers))
          (t nil))))

(defun %mop-ensure-method-table (gf qualifiers)
  "Return GF's method table for QUALIFIERS, creating it when needed."
  (let ((key (%mop-qualified-table-key qualifiers)))
    (or (gethash key gf)
        (setf (gethash key gf) (make-hash-table :test #'equal)))))

(defun %mop-invalidate-gf (gf)
  "Invalidate method-selection caches associated with GF."
  (setf *satiated-generic-functions*
        (delete gf *satiated-generic-functions* :test #'eq)
        (gethash :__satiated__ gf) nil
        (gethash :__cache-version__ gf)
        (1+ (or (gethash :__cache-version__ gf) 0)))
  (remhash :__dispatch-cache__ gf)
  gf)

(defun add-method (gf method)
  "Add METHOD to GF and invalidate dispatch metadata."
  (unless (hash-table-p gf) (error "add-method: ~S is not a generic function" gf))
  (let* ((qualifiers (method-qualifiers method))
         (specializers (method-specializers method))
         (table (%mop-ensure-method-table gf qualifiers))
         (key (%mop-normalize-specializer-key specializers)))
    (when (hash-table-p method)
      (setf (gethash :gf method) gf
            (gethash :generic-function method) gf
            (gethash :specializer method) key
            (gethash :specializers method) specializers))
    (if qualifiers
        (pushnew method (gethash key table) :test #'eq)
        (setf (gethash key table) method))
    (%mop-invalidate-gf gf)))

(defun remove-method (gf method)
  "Remove METHOD from GF and invalidate dispatch metadata."
  (unless (hash-table-p gf) (error "remove-method: ~S is not a generic function" gf))
  (dolist (table-key '(:__methods__ :__BEFORE__ :__AFTER__ :__AROUND__))
    (let ((table (gethash table-key gf)))
      (when (hash-table-p table)
        (let (updates removals)
          (maphash (lambda (key value)
                     (let ((remaining (remove method (%mop-method-value->list value) :test #'eq)))
                       (if remaining
                           (push (cons key (if (or (eq table-key :__methods__)
                                                   (= (length remaining) 1))
                                               (first remaining)
                                               remaining))
                                 updates)
                           (push key removals))))
                   table)
          (dolist (key removals)
            (remhash key table))
          (dolist (update updates)
            (setf (gethash (car update) table) (cdr update)))))))
  (%mop-invalidate-gf gf))

(defun ensure-generic-function (name &rest options
                                &key lambda-list method-class documentation
                                &allow-other-keys)
  "Return an existing or new generic-function descriptor for NAME."
  (declare (ignore options))
  (let ((gf (if (hash-table-p name)
                name
                (gethash name *mop-generic-function-registry*))))
    (unless gf
      (setf gf (make-hash-table :test #'equal)
            (gethash :__name__ gf) name
            (gethash :__methods__ gf) (make-hash-table :test #'equal)
            (gethash :__eql-index__ gf) (make-hash-table :test #'equal)
            (gethash :__method-combination__ gf) 'standard)
      (setf (gethash name *mop-generic-function-registry*) gf))
    (when lambda-list (setf (gethash :__lambda-list__ gf) lambda-list))
    (when method-class (setf (gethash :__method-class__ gf) method-class))
    (when documentation (setf (gethash :__documentation__ gf) documentation))
    gf))

(defun %vm-hashlike-slot-key (object slot-name)
  "Return the most specific hash-table key for SLOT-NAME in OBJECT."
  (when (hash-table-p object)
    (multiple-value-bind (_ found-p) (gethash slot-name object)
      (declare (ignore _))
      (cond
        (found-p slot-name)
        (t
         (let ((string-key (string-downcase (symbol-name slot-name))))
           (multiple-value-bind (_ string-found-p) (gethash string-key object)
             (declare (ignore _))
             (and string-found-p string-key))))))))

(defun slot-value-using-class (class object slot-name)
  "Read SLOT-NAME from OBJECT using CLASS metadata."
  (let ((class-slots (and (hash-table-p class) (gethash :__class-slots__ class))))
    (cond ((and class-slots (member slot-name class-slots :test #'eq))
           (gethash slot-name class))
          ((and (vectorp object) (plusp (length object)) (hash-table-p class))
           (let ((index (class-slot-vector-index class slot-name)))
             (if index (aref object index) (error "Missing slot ~S" slot-name))))
          ((hash-table-p object)
           (let ((key (%vm-hashlike-slot-key object slot-name)))
             (if key
                 (gethash key object)
                 (error "Unbound slot ~S" slot-name))))
          (t (error "slot-value-using-class: unsupported object ~S" object)))))

(defun (setf slot-value-using-class) (new-value class object slot-name)
  "Write NEW-VALUE to SLOT-NAME in OBJECT using CLASS metadata."
  (let ((class-slots (and (hash-table-p class) (gethash :__class-slots__ class))))
    (cond ((and class-slots (member slot-name class-slots :test #'eq))
           (setf (gethash slot-name class) new-value))
          ((and (vectorp object) (plusp (length object)) (hash-table-p class))
           (let ((index (class-slot-vector-index class slot-name)))
             (unless index (error "Missing slot ~S" slot-name))
             (setf (aref object index) new-value)))
          ((hash-table-p object)
           (let ((key (%vm-hashlike-slot-key object slot-name)))
             (if key
                 (setf (gethash key object) new-value)
                 (setf (gethash (string-downcase (symbol-name slot-name)) object)
                       new-value))))
          (t (error "(setf slot-value-using-class): unsupported object ~S" object)))))

(defun slot-makunbound-using-class (class object slot-name)
  "Make SLOT-NAME unbound in OBJECT using CLASS metadata and return OBJECT."
  (let ((class-slots (and (hash-table-p class) (gethash :__class-slots__ class))))
    (cond ((and class-slots (member slot-name class-slots :test #'eq))
           (remhash slot-name class))
          ((and (vectorp object) (plusp (length object)) (hash-table-p class))
           (let ((index (class-slot-vector-index class slot-name)))
             (unless index (error "Missing slot ~S" slot-name))
             (setf (aref object index) *unbound-slot-marker*)))
          ((hash-table-p object)
           (let ((key (%vm-hashlike-slot-key object slot-name)))
             (if key
                 (remhash key object)
                 (remhash (string-downcase (symbol-name slot-name)) object))))
          (t (error "slot-makunbound-using-class: unsupported object ~S" object))))
  object)
