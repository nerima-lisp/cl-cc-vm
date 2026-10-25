(in-package :cl-cc/vm)

;;; Class and generic-function introspection: obj-class-ht/class-slots-of
;;; routing, initarg application, metaclass predicates, package-independent
;;; generic-function-name comparison (needed because guest DEFMETHOD and VM
;;; protocol dispatch read symbols in different packages), the synchronous
;;; standard method-combination caller, and direct-slot/CPL queries. Built
;;; on vm-clos-slots-storage.lisp (loads before this file).

(defun %vm-obj-class-ht (obj-ht)
  "Return the class hash-table for an instance OBJ-HT, or NIL."
  (cond
    ((%vm-vector-instance-p obj-ht)
      (%vm-update-obsolete-instance obj-ht)
      (aref obj-ht 0))
    ((%vm-hashlike-storage obj-ht)
      (%vm-update-obsolete-instance obj-ht)
      (%vm-hashlike-gethash :__class__ obj-ht))))

(defun %vm-class-slots-of (obj-ht)
  "Return (values class-ht class-slots) for class-allocation routing."
  (let ((own-class-slots (%vm-hashlike-gethash :__class-slots__ obj-ht)))
    (if own-class-slots (values obj-ht own-class-slots)
      (let ((class-ht (%vm-obj-class-ht obj-ht)))
        (values
          class-ht
          (when class-ht
            (%vm-hashlike-gethash :__class-slots__ class-ht)))))))

(defun %vm-apply-initarg (initarg-key value initarg-map class-slots class-ht obj-ht)
  "Write VALUE to the slot for INITARG-KEY, routing class-allocated slots to CLASS-HT."
  (let ((slot-entry (assoc initarg-key initarg-map)))
    (when slot-entry
      (let ((slot-name (cdr slot-entry)))
        (if (member slot-name class-slots :test #'eq) (setf (gethash slot-name class-ht) value)
          (%vm-raw-slot-write class-ht class-slots obj-ht slot-name value))))))

(defun %vm-standard-metaclass-p (metaclass)
  "Return T when METACLASS denotes the built-in standard metaclass path."
  (or
    (null metaclass)
    (eq metaclass 'standard-class)
    (and
      (hash-table-p metaclass)
      (eq (gethash :__name__ metaclass) 'standard-class))))

(defun %vm-resolve-class-designator (designator state)
  "Resolve a class DESIGNATOR (symbol or descriptor) to a class hash table when possible."
  (cond
    ((hash-table-p designator) designator)
    ((symbolp designator) (gethash designator (vm-class-registry state)))
    (t nil)))

(defun %vm-class-effective-metaclass (class-ht state)
  "Return the effective metaclass descriptor or symbol for CLASS-HT."
  (let ((metaclass (and (hash-table-p class-ht) (gethash :__metaclass__ class-ht))))
    (or (%vm-resolve-class-designator metaclass state) metaclass)))

(defun %vm-class-nonstandard-metaclass-p (class-ht state)
  "Return T when CLASS-HT has a custom metaclass."
  (not (%vm-standard-metaclass-p (%vm-class-effective-metaclass class-ht state))))

(defun %vm-same-function-name-p (left right)
  "Return true when LEFT and RIGHT denote the same package-independent name."
  (cond
    ((and (symbolp left) (symbolp right))
      (string= (symbol-name left) (symbol-name right)))
    ((and
        (consp left)
        (consp right)
        (consp (cdr left))
        (consp (cdr right))
        (null (cddr left))
        (null (cddr right))
        (eq (car left) 'setf)
        (eq (car right) 'setf)
        (symbolp (cadr left))
        (symbolp (cadr right)))
      (string= (symbol-name (cadr left)) (symbol-name (cadr right))))
    (t nil)))

(defun %vm-same-name-global-generic-function (state name)
  "Return a generic function stored under any same-named function name in STATE.

The VM looks protocol generic functions up with symbols read in :CL-CC/VM, while
guest source is read in :CL-CC — so DEFMETHOD registered
CL-CC::SLOT-VALUE-USING-CLASS while VM-SLOT-READ asked for
CL-CC/VM::SLOT-VALUE-USING-CLASS and found nothing. SETF function names need the
same package-independent comparison for VM-SLOT-WRITE."
  (let ((found nil))
    (maphash
      (lambda (key value)
        (when (and
            (null found)
            (%vm-same-function-name-p key name)
            (vm-generic-function-p value))
          (setf found value)))
      (vm-global-vars state))
    found))

(defun %vm-global-generic-function (state name)
  "Return global generic function NAME, or NIL if it is not available."
  (let ((value
        (or
          (gethash name (vm-global-vars state))
          (when (consp name)
            (let ((matched nil))
              (maphash
                (lambda (key candidate)
                  (when (and (null matched) (equal key name))
                    (setf matched candidate)))
                (vm-global-vars state))
              matched)))))
    (if (vm-generic-function-p value) value
      (%vm-same-name-global-generic-function state name))))

(defun %vm-call-generic-sync (gf-ht state args &key default)
  "Synchronously call GF-HT with ARGS using the standard method combination.
DEFAULT is called when no primary method is applicable.  This helper is used by
VM primitives that need protocol hooks without introducing new instructions."
  (let* ((primary-methods (vm-get-all-applicable-methods gf-ht state args))
         (before-methods (%lookup-qualified-methods gf-ht :__BEFORE__ state args))
         (after-methods (%lookup-qualified-methods gf-ht :__AFTER__ state args))
         (around-methods (%lookup-qualified-methods gf-ht :__AROUND__ state args)))
    (cond
      (around-methods
        (%vm-call-closure-sync (%vm-method-function (car around-methods)) state args))
      (t
        (dolist (method before-methods)
          (%vm-call-closure-sync (%vm-method-function method) state args))
        (let ((result
              (if primary-methods (%vm-call-closure-sync
                  (%vm-method-function (car primary-methods))
                  state
                  args
                  :method-context
                  (list gf-ht primary-methods args))
                (and default (funcall default)))))
          (dolist (method (reverse after-methods))
            (%vm-call-closure-sync (%vm-method-function method) state args))
          result)))))

(defun %vm-direct-primary-method-p (gf-ht key)
  "Return T when GF-HT has a direct primary protocol method for KEY."
  (let ((methods-ht
        (and
          (hash-table-p gf-ht)
          (nth-value 1 (gethash :__methods__ gf-ht))
          (gethash :__methods__ gf-ht))))
    (and
      methods-ht
      (loop for specializers being the hash-keys of methods-ht using (hash-value method)
            thereis (and
          (consp specializers)
          (eq (first specializers) key)
          (every
            (lambda (specializer)
              (eq specializer t))
            (rest specializers))
          (hash-table-p method)
          (null (gethash :qualifiers method))
          (eq (gethash :gf method) gf-ht))))))

(defun %vm-class-direct-slot-p (class-ht slot-name)
  "Return T when CLASS-HT directly declares SLOT-NAME."
  (and
    (hash-table-p class-ht)
    (member slot-name (gethash :__direct-slots__ class-ht) :test #'eq)
    t))

(defun %vm-class-slot-initargs-for-slot (class-ht slot-name)
  "Return all initargs in CLASS-HT metadata that initialize SLOT-NAME."
  (let ((result nil))
    (dolist (entry (and (hash-table-p class-ht) (gethash :__initargs__ class-ht)))
      (when (eq (cdr entry) slot-name)
        (pushnew (car entry) result :test #'eq)))
    (nreverse result)))
