(in-package #:cl-cc/vm/clos)

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

(defun %vm-collect-applicable-method-entries
    (methods-ht state all-args &optional scalar-rest-p)
  "Collect applicable method and dispatch-key pairs in lexicographic order."
  (let* ((result nil)
         (seen nil)
         (cpls (%vm-arg-cpls state all-args)))
    (labels ((collect-methods (key value)
               (dolist (method (%vm-method-value->list value))
                 (unless (some (lambda (entry)
                                 (and (eq (cdr entry) method)
                                      (or (hash-table-p method)
                                          (equal (car entry) key))))
                               seen)
                   (let ((entry (cons key method)))
                     (push entry seen)
                     (push entry result)))))
             (key-matches-args-p (key)
               (let ((specializers (if (%eql-specializer-p key)
                                       (list key)
                                       key)))
                 (and (listp specializers)
                      (= (length specializers) (length all-args))
                      (loop for specializer in specializers
                            for arg in all-args
                            for cpl in cpls
                            always
                              (or (eq specializer t)
                                  (if (%eql-specializer-p specializer)
                                      (%eql-specializer-matches-p specializer arg)
                                      (or (member specializer cpl :test (function equal))
                                          (vm-specializer-matches-class-p
                                           specializer
                                           (vm-classify-arg arg state)))))))))
             (entry-specializers (entry)
               (let* ((method (cdr entry))
                      (specializer
                        (or (and (hash-table-p method)
                                 (or (gethash :specializers method)
                                     (gethash :specializer method)))
                            (car entry))))
                 (cond
                   ((%eql-specializer-p specializer) (list specializer))
                   ((listp specializer) specializer)
                   (t (list specializer)))))
             (entry-rank (entry)
               (let ((specializers (entry-specializers entry)))
                 (if (= (length specializers) (length all-args))
                     (loop for specializer in specializers
                           for arg in all-args
                           for cpl in cpls
                           collect
                             (if (%eql-specializer-p specializer)
                                 -1
                                 (or (position specializer cpl :test (function equal))
                                     (position
                                      specializer
                                      (vm-builtin-class-precedence-list
                                       (vm-classify-arg arg state))
                                      :test (function equal))
                                     most-positive-fixnum)))
                     (make-list (length all-args)
                                :initial-element most-positive-fixnum))))
             (rank< (left right)
               (loop for left-rank in (entry-rank left)
                     for right-rank in (entry-rank right)
                     when (< left-rank right-rank) do (return t)
                     when (> left-rank right-rank) do (return nil)
                     finally (return nil))))
      (when (hash-table-p methods-ht)
        (maphash
         (lambda (key value)
           (when (and (%vm-key-has-eql-specializer-p key)
                      (key-matches-args-p key))
             (collect-methods key value)))
         methods-ht)
        (dolist (combo (%vm-dispatch-key-combinations cpls))
          (let ((key (%vm-canonical-dispatch-key combo)))
            (collect-methods key (gethash key methods-ht))))
        (when (and scalar-rest-p (cdr all-args))
          (dolist (class (car cpls))
            (collect-methods class (gethash class methods-ht)))))
      (stable-sort (nreverse result) (function rank<)))))
(defun %vm-global-generic-function (state name &optional (args nil args-supplied-p))
  "Return a same-named generic function, preferring its best applicable method."
  (let* ((globals (vm-global-vars state))
         (value (or (gethash name globals)
                    (when (consp name)
                      (let ((matched nil))
                        (maphash (lambda (key candidate)
                                   (when (and (null matched) (equal key name))
                                     (setf matched candidate)))
                                 globals)
                        matched))))
         (fallback (if (vm-generic-function-p value)
                       value
                       (%vm-same-name-global-generic-function state name))))
    (unless args-supplied-p
      (return-from %vm-global-generic-function fallback))
    (let ((cpls (%vm-arg-cpls state args))
          (best-gf nil)
          (best-rank nil)
          (seen nil))
      (labels ((function-name-symbol (function-name)
                 (if (and (consp function-name) (eq (car function-name) (quote setf)))
                     (cadr function-name)
                     function-name))
               (function-name-package (function-name)
                 (let* ((symbol (function-name-symbol function-name))
                        (package (and (symbolp symbol) (symbol-package symbol))))
                   (if package (package-name package) "")))
               (candidate-key< (left right)
                 (let ((left-key (car left))
                       (right-key (car right)))
                   (cond
                     ((equal left-key name) (not (equal right-key name)))
                     ((equal right-key name) nil)
                     ((not (string= (function-name-package left-key)
                                    (function-name-package right-key)))
                      (string< (function-name-package left-key)
                               (function-name-package right-key)))
                     (t (< (sxhash left-key) (sxhash right-key))))))
               (method-rank (entry)
                 (let* ((method (cdr entry))
                        (key (car entry))
                        (specializer
                          (if (hash-table-p method)
                              (or (gethash :specializers method)
                                  (gethash :specializer method)
                                  key)
                              key))
                        (specializers (if (or (%eql-specializer-p specializer)
                                              (not (listp specializer)))
                                          (list specializer)
                                          specializer)))
                   (if (= (length specializers) (length args))
                       (loop for item in specializers
                             for cpl in cpls
                             collect (if (%eql-specializer-p item)
                                         -1
                                         (or (position item cpl :test (function equal))
                                             most-positive-fixnum)))
                       :inapplicable)))
               (rank< (left right)
                 (loop for left-rank in left
                       for right-rank in right
                       when (< left-rank right-rank) do (return t)
                       when (> left-rank right-rank) do (return nil)
                       finally (return nil)))
               (candidate-table-method-entries (candidate table-key)
                 (%vm-collect-applicable-method-entries
                  (gethash table-key candidate)
                  state
                  args
                  (%vm-single-required-rest-lambda-list-p
                   (gethash :__lambda-list__ candidate))))
               (candidate-method-entries (candidate)
                 (append (candidate-table-method-entries candidate :__AROUND__)
                         (candidate-table-method-entries candidate :__methods__)
                         (candidate-table-method-entries candidate :__BEFORE__)
                         (candidate-table-method-entries candidate :__AFTER__)))
               (consider (candidate)
                 (unless (member candidate seen :test (function eq))
                   (push candidate seen)
                   (when (vm-generic-function-p candidate)
                     (let* ((methods (candidate-method-entries candidate))
                            (ranks (remove :inapplicable
                                           (mapcar (function method-rank) methods))))
                       (when ranks
                         (let ((rank (reduce (lambda (left right)
                                               (if (rank< right left) right left))
                                             ranks)))
                           (when (or (null best-gf) (rank< rank best-rank))
                             (setf best-gf candidate
                                   best-rank rank)))))))))
        (let ((candidates nil))
          (maphash (lambda (key candidate)
                     (when (and (%vm-same-function-name-p key name)
                                (vm-generic-function-p candidate))
                       (push (cons key candidate) candidates)))
                   globals)
          (dolist (entry (stable-sort candidates (function candidate-key<)))
            (consider (cdr entry))))
        (or best-gf fallback)))))

(defun %vm-call-generic-sync (gf-ht state args &key default)
  "Synchronously call GF-HT using the standard method combination."
  (let* ((scalar-rest-p
           (%vm-single-required-rest-lambda-list-p
            (gethash :__lambda-list__ gf-ht)))
         (primary-entries
           (%vm-collect-applicable-method-entries
            (gethash :__methods__ gf-ht) state args scalar-rest-p))
         (before-entries
           (%vm-collect-applicable-method-entries
            (gethash :__BEFORE__ gf-ht) state args scalar-rest-p))
         (after-entries
           (%vm-collect-applicable-method-entries
            (gethash :__AFTER__ gf-ht) state args scalar-rest-p))
         (around-entries
           (%vm-collect-applicable-method-entries
            (gethash :__AROUND__ gf-ht) state args scalar-rest-p))
         (primary-methods (mapcar (function cdr) primary-entries))
         (before-methods (mapcar (function cdr) before-entries))
         (after-methods (mapcar (function cdr) after-entries))
         (around-methods (mapcar (function cdr) around-entries)))
    (labels ((applicable-method-entries (table-key call-args)
               (%vm-collect-applicable-method-entries
                (gethash table-key gf-ht)
                state
                call-args
                scalar-rest-p))
             (method-entry-specializers (entry)
               (let* ((method (cdr entry))
                      (specializers
                        (and (hash-table-p method)
                             (method-specializers method)))
                      (key (car entry)))
                 (or specializers
                     (cond
                       ((%eql-specializer-p key) (list key))
                       ((listp key) key)
                       (t (list key))))))
             (method-specializers-equal-p (left right)
               (let ((left-specializers (method-entry-specializers left))
                     (right-specializers (method-entry-specializers right)))
                 (and (= (length left-specializers) (length right-specializers))
                      (every
                       (lambda (left-specializer right-specializer)
                         (cond
                           ((and (%eql-specializer-p left-specializer)
                                 (%eql-specializer-p right-specializer))
                            (eql (second left-specializer)
                                 (second right-specializer)))
                           ((or (%eql-specializer-p left-specializer)
                                (%eql-specializer-p right-specializer))
                            nil)
                           (t (eq left-specializer right-specializer))))
                       left-specializers
                       right-specializers))))
             (method-groups (entries)
               (let ((groups nil))
                 (dolist (entry entries)
                   (let ((group
                          (find-if
                           (lambda (group)
                             (method-specializers-equal-p entry (car group)))
                           groups)))
                     (if group
                         (push entry (cdr group))
                         (push (cons entry (list entry)) groups))))
                 (nreverse groups)))
             (same-method-members-p (left right)
               (and (= (length left) (length right))
                    (every
                     (lambda (entry)
                       (member (cdr entry) right
                               :test (function eq)
                               :key (function cdr)))
                     left)))
             (same-method-sequences-p (left right)
               (let ((left-groups (method-groups left))
                     (right-groups (method-groups right)))
                 (and (= (length left-groups) (length right-groups))
                      (loop for left-group in left-groups
                            for right-group in right-groups
                            always (and
                                    (method-specializers-equal-p
                                     (car left-group)
                                     (car right-group))
                                    (same-method-members-p
                                     (cdr left-group)
                                     (cdr right-group)))))))
             (same-applicable-method-set-p (next-args)
               (and
                (same-method-sequences-p
                 around-entries
                 (applicable-method-entries :__AROUND__ next-args))
                (same-method-sequences-p
                 before-entries
                 (applicable-method-entries :__BEFORE__ next-args))
                (same-method-sequences-p
                 primary-entries
                 (applicable-method-entries :__methods__ next-args))
                (same-method-sequences-p
                 after-entries
                 (applicable-method-entries :__AFTER__ next-args))))
             (call-primary (methods call-args)
               (if methods
                   (%vm-call-closure-sync
                    (%vm-method-function (car methods))
                    state
                    call-args
                    :method-context
                    (if (cdr methods)
                        (list gf-ht
                              methods
                              call-args
                              :sync-next
                              (lambda (next-args)
                                (unless
                                    (same-applicable-method-set-p next-args)
                                  (error
                                   "call-next-method: changed arguments select a different set of applicable methods"))
                                (call-primary (cdr methods) next-args)))
                        (list gf-ht methods call-args)))
                   (if default
                       (funcall default)
                       (error "no-primary-method: no primary method available"))))
             (call-core (call-args)
               (dolist (method before-methods)
                 (%vm-call-closure-sync
                  (%vm-method-function method) state call-args))
               (let ((result (call-primary primary-methods call-args)))
                 (dolist (method (reverse after-methods))
                   (%vm-call-closure-sync
                    (%vm-method-function method) state call-args))
                 result))
             (call-around (methods call-args)
               (if methods
                   (let* ((next-around (cdr methods))
                          (has-next-p (or next-around primary-methods default))
                          (method-context
                           (if has-next-p
                               (list gf-ht
                                     nil
                                     call-args
                                     :phase
                                     :around
                                     :sync-next
                                     (lambda (next-args)
                                       (unless
                                           (same-applicable-method-set-p next-args)
                                         (error
                                          "call-next-method: changed arguments select a different set of applicable methods"))
                                       (call-around next-around next-args)))
                               (list gf-ht nil call-args :phase :around))))
                     (%vm-call-closure-sync
                      (%vm-method-function (car methods))
                      state
                      call-args
                      :method-context
                      method-context))
                   (call-core call-args))))
      (call-around around-methods args))))

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
          (or (eq specializers key)
              (and
                (consp specializers)
                (eq (first specializers) key)
                (every
                  (lambda (specializer)
                    (eq specializer t))
                  (rest specializers))))
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
