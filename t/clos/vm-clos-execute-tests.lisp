;;;; tests/unit/vm/vm-clos-execute-tests.lisp
;;;; Coverage for src/vm/vm-clos-execute.lisp:
;;;;   execute-instruction for vm-class-def, vm-make-obj,
;;;;   vm-slot-read, vm-slot-write, vm-slot-boundp,
;;;;   vm-slot-makunbound, vm-slot-exists-p.

(in-package #:cl-cc/vm/clos/test)

;;; ─── Helpers ──────────────────────────────────────────────────────────────

(defun make-clos-vm ()
  "Fresh vm-state for CLOS instruction tests."
  (make-test-vm))
(defun class-def-inst (dst name &key (supers '()) (slots '()) (initargs '()) (cl-cc/vm/clos:class-slots '()))
  "Build a vm-class-def instruction for simple class registration."
  (cl-cc/vm:make-vm-class-def
   :dst dst
   :class-name name
   :superclasses supers
   :slot-names slots
   :slot-initargs initargs
   :slot-initform-regs nil
   :default-initarg-regs nil
   :class-slots cl-cc/vm/clos:class-slots))

(defun exec-class-def (s dst name &key (supers '()) (slots '()) (initargs '()) (cl-cc/vm/clos:class-slots '()))
  "Execute a vm-class-def and return the new PC."
  (values (cl-cc/vm/protocols:execute-instruction
           (class-def-inst dst name :supers supers :slots slots :initargs initargs :class-slots cl-cc/vm/clos:class-slots)
           s 0 nil)))
(defun make-cdef-for-test (name &key (supers '()) (default-initarg-regs nil) (cl-cc/vm/clos:class-slots nil))
  "Build a vm-class-def with empty slot/initarg lists and explicit defaults for testing."
  (cl-cc/vm:make-vm-class-def
   :dst :R0 :class-name name :superclasses supers
   :slot-names '() :slot-initargs '() :slot-initform-regs nil
   :default-initarg-regs default-initarg-regs :class-slots cl-cc/vm/clos:class-slots))

(defun test-instance-class (instance)
  "Return INSTANCE's class descriptor for either supported instance layout."
  (cond
    ((hash-table-p instance) (gethash :__class__ instance))
    ((and (vectorp instance) (> (cl-cc/vm/data:length instance) 0)) (aref instance 0))))

(defun test-slot-index (class slot-name)
  "Return SLOT-NAME's vector index in CLASS."
  (let ((location (cdr (assoc slot-name (gethash :__slot-locations__ class) :test #'eq))))
    (and location (1+ location))))

(defun test-instance-slot-value (instance slot-name)
  "Return SLOT-NAME from INSTANCE using the active raw layout."
  (let ((class (test-instance-class instance)))
    (if (hash-table-p instance)
        (gethash slot-name instance)
        (cl-cc/vm/clos::slot-value-by-index instance (test-slot-index class slot-name)))))

(defun test-instance-slot-bound-p (instance slot-name)
  "Return whether SLOT-NAME is bound in INSTANCE using VM raw slot logic."
  (multiple-value-bind (class cl-cc/vm/clos:class-slots) (cl-cc/vm/clos::%vm-class-slots-of instance)
    (cl-cc/vm/clos::%vm-raw-slot-boundp class cl-cc/vm/clos:class-slots instance slot-name)))

(defun test-instance-makunbound (instance slot-name)
  "Make SLOT-NAME unbound in INSTANCE using VM raw slot logic."
  (multiple-value-bind (class cl-cc/vm/clos:class-slots) (cl-cc/vm/clos::%vm-class-slots-of instance)
    (cl-cc/vm/clos::%vm-raw-slot-makunbound class cl-cc/vm/clos:class-slots instance slot-name)))

;;; ─── vm-class-def / vm-make-obj ──────────────────────────────────────────

;;; ─── vm-slot-read / vm-slot-write / vm-slot-boundp / vm-slot-makunbound ─

(defun make-test-instance (s class-reg obj-reg class-name slots)
  "Register CLASS-NAME with SLOTS and create an instance, storing in OBJ-REG."
  (exec-class-def s class-reg class-name :slots slots)
  (cl-cc/vm/protocols:execute-instruction
   (cl-cc/vm:make-vm-make-obj :dst obj-reg :class-reg class-reg :initarg-regs nil)
   s 0 nil))

;;; ─── vm-class-def helper functions ──────────────────────────────────────

(defun %make-reg-with-base (slots &key (initargs nil))
  "Build a class registry HT containing a single 'base class with given SLOTS."
  (let ((reg (make-hash-table :test #'eq))
        (base-ht (make-hash-table :test #'eq)))
    (setf (gethash :__slots__ base-ht) slots
          (gethash :__superclasses__ base-ht) nil)
    (when initargs (setf (gethash :__initargs__ base-ht) initargs))
    (setf (gethash 'base reg) base-ht)
    reg))

;;; ─── copy-instance layout coverage ───────────────────────────────────────

;;; ─── vm-slot-exists-p / vm-class-name-fn / vm-class-of-fn / vm-find-class

;;; ─── vm-generic-call inline cache ─────────────────────────────────────────



(defun register-vm-clos-execute-tests ()
  (it-sequential "vm-raw-slot-write-prefers-existing-symbol-slot"
                 (let ((obj-ht (make-hash-table :test #'equal)))
                   (setf (gethash 'n obj-ht) 1
                         (gethash "n" obj-ht) 0)
                   (cl-cc/vm/clos::%vm-raw-slot-write nil nil obj-ht 'n 7)
                   (expect (= 7 (gethash 'n obj-ht)) :to-be-truthy)
                   (expect (= 0 (gethash "n" obj-ht)) :to-be-truthy)))

  (it-sequential "vm-slot-read-signals-error-when-unbound"
                 (let ((s (make-clos-vm)))
                   (make-test-instance s :R0 :R1 'thing '(val))
                   (test-instance-makunbound (cl-cc/vm:vm-reg-get s :R1) 'val)
                   (signals error (cl-cc/vm/protocols:execute-instruction
                                   (cl-cc/vm:make-vm-slot-read :dst :R2 :obj-reg :R1 :slot-name 'val)
                                   s 0 nil))))

  (it-sequential "vm-slot-write-advances-pc"
                 (let ((s (make-clos-vm)))
                   (make-test-instance s :R0 :R1 'c '(v))
                   (cl-cc/vm:vm-reg-set s :R2 0)
                   (let ((new-pc (first (multiple-value-list
                                         (cl-cc/vm/protocols:execute-instruction
                                          (cl-cc/vm:make-vm-slot-write :obj-reg :R1 :slot-name 'v :value-reg :R2)
                                          s 7 nil)))))
                     (expect (= 8 new-pc) :to-be-truthy))))

  (it-sequential "vm-slot-boundp-bound-and-unbound bound"
                 (destructuring-bind (remove-p) (list nil)
                   (let ((s (make-clos-vm)))
                     (make-test-instance s :R0 :R1 'car '(model))
                     (when remove-p (test-instance-makunbound (cl-cc/vm:vm-reg-get s :R1) 'model))
                     (cl-cc/vm/protocols:execute-instruction
                      (cl-cc/vm:make-vm-slot-boundp :dst :R2 :obj-reg :R1 :slot-name-sym 'model)
                      s 0 nil)
                     (if remove-p
                         (expect (cl-cc/vm:vm-reg-get s :R2) :to-be-falsy)
                       (expect (cl-cc/vm:vm-reg-get s :R2) :to-be-truthy)))))

  (it-sequential "vm-slot-boundp-bound-and-unbound unbound"
                 (destructuring-bind (remove-p) (list t)
                   (let ((s (make-clos-vm)))
                     (make-test-instance s :R0 :R1 'car '(model))
                     (when remove-p (test-instance-makunbound (cl-cc/vm:vm-reg-get s :R1) 'model))
                     (cl-cc/vm/protocols:execute-instruction
                      (cl-cc/vm:make-vm-slot-boundp :dst :R2 :obj-reg :R1 :slot-name-sym 'model)
                      s 0 nil)
                     (if remove-p
                         (expect (cl-cc/vm:vm-reg-get s :R2) :to-be-falsy)
                       (expect (cl-cc/vm:vm-reg-get s :R2) :to-be-truthy)))))

  (it-sequential "vm-slot-boundp-uses-class-slot-storage"
                 (let ((s (make-clos-vm)))
                   (exec-class-def s :R0 'shared-box :slots '(shared) :class-slots '(shared))
                   (cl-cc/vm/protocols:execute-instruction
                    (cl-cc/vm:make-vm-make-obj :dst :R1 :class-reg :R0 :initarg-regs nil)
                    s 0 nil)
                   (cl-cc/vm/protocols:execute-instruction
                    (cl-cc/vm:make-vm-slot-boundp :dst :R2 :obj-reg :R1 :slot-name-sym 'shared)
                    s 0 nil)
                   (expect (cl-cc/vm:vm-reg-get s :R2) :to-be-truthy)
                   (remhash 'shared (cl-cc/vm:vm-reg-get s :R0))
                   (cl-cc/vm/protocols:execute-instruction
                    (cl-cc/vm:make-vm-slot-boundp :dst :R2 :obj-reg :R1 :slot-name-sym 'shared)
                    s 0 nil)
                   (expect (cl-cc/vm:vm-reg-get s :R2) :to-be-falsy)))

  (it-sequential "vm-slot-boundp-migrates-obsolete-instance"
                 (let ((s (make-clos-vm)))
                   (exec-class-def s :R0 'redef :slots '(x))
                   (cl-cc/vm/protocols:execute-instruction
                    (cl-cc/vm:make-vm-make-obj :dst :R1 :class-reg :R0 :initarg-regs nil)
                    s 0 nil)
                   (exec-class-def s :R3 'redef :slots '(y))
                   (cl-cc/vm/protocols:execute-instruction
                    (cl-cc/vm:make-vm-slot-boundp :dst :R2 :obj-reg :R1 :slot-name-sym 'y)
                    s 0 nil)
                   (expect (cl-cc/vm:vm-reg-get s :R2) :to-be-truthy)
                   (cl-cc/vm/protocols:execute-instruction
                    (cl-cc/vm:make-vm-slot-boundp :dst :R2 :obj-reg :R1 :slot-name-sym 'x)
                    s 0 nil)
                   (expect (cl-cc/vm:vm-reg-get s :R2) :to-be-falsy)))

  (it-sequential "vm-slot-makunbound-removes-key-and-stores-obj"
                 (let ((s (make-clos-vm)))
                   (make-test-instance s :R0 :R1 'node '(data))
                   (cl-cc/vm/protocols:execute-instruction
                    (cl-cc/vm:make-vm-slot-makunbound :dst :R2 :obj-reg :R1 :slot-name-sym 'data)
                    s 0 nil)
                   (expect (test-instance-slot-bound-p (cl-cc/vm:vm-reg-get s :R1) 'data) :to-be-falsy)
                   (expect (cl-cc/vm:vm-reg-get s :R2) :to-be (cl-cc/vm:vm-reg-get s :R1))))

  (it-sequential "vm-slot-makunbound-removes-class-slot-storage"
                 (let ((s (make-clos-vm)))
                   (exec-class-def s :R0 'shared-node :slots '(shared) :class-slots '(shared))
                   (cl-cc/vm/protocols:execute-instruction
                    (cl-cc/vm:make-vm-make-obj :dst :R1 :class-reg :R0 :initarg-regs nil)
                    s 0 nil)
                   (cl-cc/vm/protocols:execute-instruction
                    (cl-cc/vm:make-vm-slot-makunbound :dst :R2 :obj-reg :R1 :slot-name-sym 'shared)
                    s 0 nil)
                   (expect (nth-value 1 (gethash 'shared (cl-cc/vm:vm-reg-get s :R0))) :to-be-falsy)
                   (expect (test-instance-slot-bound-p (cl-cc/vm:vm-reg-get s :R1) 'shared) :to-be-falsy)
                   (expect (cl-cc/vm:vm-reg-get s :R2) :to-be (cl-cc/vm:vm-reg-get s :R1))))

  (it-sequential "vm-slot-makunbound-migrates-obsolete-instance"
                 (let ((s (make-clos-vm)))
                   (exec-class-def s :R0 'makun-redef :slots '(x))
                   (cl-cc/vm/protocols:execute-instruction
                    (cl-cc/vm:make-vm-make-obj :dst :R1 :class-reg :R0 :initarg-regs nil)
                    s 0 nil)
                   (exec-class-def s :R3 'makun-redef :slots '(y))
                   (cl-cc/vm/protocols:execute-instruction
                    (cl-cc/vm:make-vm-slot-makunbound :dst :R2 :obj-reg :R1 :slot-name-sym 'y)
                    s 0 nil)
                   (expect (test-instance-class (cl-cc/vm:vm-reg-get s :R1)) :to-be (cl-cc/vm:vm-reg-get s :R3))
                   (expect (test-instance-slot-bound-p (cl-cc/vm:vm-reg-get s :R1) 'y) :to-be-falsy)))

  (it-sequential "vm-cdef-collect-slots-merges-inherited"
                 (let* ((reg (%make-reg-with-base '(a b)))
                        (slots (cl-cc/vm/clos::%vm-cdef-collect-slots '(base) '(b c) reg)))
                   (expect (member 'a slots) :to-be-truthy)
                   (expect (member 'b slots) :to-be-truthy)
                   (expect (member 'c slots) :to-be-truthy)
                   (expect (= 1 (count 'b slots)) :to-be-truthy)))

  (it-sequential "vm-cdef-collect-initargs-merges-and-deduplicates"
                 (let* ((reg (%make-reg-with-base '(x) :initargs '((:x-arg . x))))
                        (iargs (cl-cc/vm/clos::%vm-cdef-collect-initargs '(base) '((:x-arg . x) (:y-arg . y)) reg)))
                   (expect (assoc :x-arg iargs) :to-be-truthy)
                   (expect (assoc :y-arg iargs) :to-be-truthy)
                   (expect (= 1 (count :x-arg iargs :key #'car)) :to-be-truthy)))

  (it-sequential "vm-cdef-collect-default-initargs-from-regs"
                 (let* ((s    (make-clos-vm))
                        (inst (make-cdef-for-test 'foo :default-initarg-regs '((:size . :R1))))
                        (reg))
                   (cl-cc/vm:vm-reg-set s :R1 42)
                   (let ((result (cl-cc/vm/clos::%vm-cdef-collect-default-initargs inst '() reg s)))
                     (expect (= 1 (cl-cc/vm/data:length result)) :to-be-truthy)
                     (expect (= 42 (cdr (assoc :size result))) :to-be-truthy))))

  (it-sequential "vm-cdef-collect-default-initargs-overrides-super"
                 (let* ((s       (make-clos-vm))
                        (base-ht (make-hash-table :test #'eq))
                        (reg     (cl-cc/vm/execution:vm-class-registry s))
                        (inst    (make-cdef-for-test 'child :supers '(base)
                                                     :default-initarg-regs '((:width . :R2)))))
                   (setf (gethash :__default-initargs__ base-ht) '((:width . 10) (:height . 20))
                         (gethash 'base reg) base-ht)
                   (cl-cc/vm:vm-reg-set s :R2 99)
                   (let ((result (cl-cc/vm/clos::%vm-cdef-collect-default-initargs inst '(base) reg s)))
                     (expect (= 99 (cdr (assoc :width result))) :to-be-truthy)
                     (expect (= 1 (count :width result :key #'car)) :to-be-truthy)
                     (expect (assoc :height result) :to-be-truthy))))

  (it-sequential "vm-cdef-collect-class-slots-merges-inherited"
                 (let* ((s       (make-clos-vm))
                        (base-ht (make-hash-table :test #'eq))
                        (reg     (cl-cc/vm/execution:vm-class-registry s))
                        (inst    (make-cdef-for-test 'child :supers '(base) :class-slots '(count))))
                   (setf (gethash :__class-slots__ base-ht) '(shared)
                         (gethash 'base reg) base-ht)
                   (let ((result (cl-cc/vm/clos::%vm-cdef-collect-class-slots inst '(base) reg)))
                     (expect (member 'count result) :to-be-truthy)
                     (expect (member 'shared result) :to-be-truthy))))

  (it-sequential "vm-cdef-collect-class-slots-no-supers"
                 (let* ((reg  (make-hash-table :test #'eq))
                        (inst (make-cdef-for-test 'leaf :class-slots '(x y))))
                   (expect (cl-cc/vm/clos::%vm-cdef-collect-class-slots inst '() reg) :to-equal '(x y))))

  (it-sequential "vm-cdef-init-class-slots-initializes-fresh-slots"
                 (let ((class-ht (make-hash-table :test #'eq)))
                   (cl-cc/vm/clos::%vm-cdef-init-class-slots class-ht '(x y) '((x . 1) (y . 2)))
                   (expect (= 1 (gethash 'x class-ht)) :to-be-truthy)
                   (expect (= 2 (gethash 'y class-ht)) :to-be-truthy)))

  (it-sequential "vm-cdef-init-class-slots-does-not-overwrite-existing"
                 (let ((class-ht (make-hash-table :test #'eq)))
                   (setf (gethash 'x class-ht) 99)
                   (cl-cc/vm/clos::%vm-cdef-init-class-slots class-ht '(x) '((x . 1)))
                   (expect (= 99 (gethash 'x class-ht)) :to-be-truthy)))

  (it-sequential "vm-cdef-init-class-slots-nil-for-missing-alist"
                 (let ((class-ht (make-hash-table :test #'eq)))
                   (cl-cc/vm/clos::%vm-cdef-init-class-slots class-ht '(z) '())
                   (expect (gethash 'z class-ht) :to-be-null)))

  (it-sequential "vm-obj-class-ht-returns-class-entry"
                 (let* ((class-ht (make-hash-table :test #'eq))
                        (obj-ht   (make-hash-table :test #'eq)))
                   (setf (gethash :__class__ obj-ht) class-ht)
                   (expect (cl-cc/vm/clos::%vm-obj-class-ht obj-ht) :to-be class-ht)))

  (it-sequential "vm-obj-class-ht-nil-for-non-instance integer"
                 (destructuring-bind (obj) (list 42)
                   (expect (cl-cc/vm/clos::%vm-obj-class-ht obj) :to-be-null)))

  (it-sequential "vm-obj-class-ht-nil-for-non-instance bare-ht"
                 (destructuring-bind (obj) (list (make-hash-table :test #'eq))
                   (expect (cl-cc/vm/clos::%vm-obj-class-ht obj) :to-be-null)))

  (it-sequential "vm-class-slots-of-returns-class-and-slots"
                 (let* ((class-ht (make-hash-table :test #'eq))
                        (obj-ht   (make-hash-table :test #'eq)))
                   (setf (gethash :__class-slots__ class-ht) '(shared-slot)
                         (gethash :__class__ obj-ht) class-ht)
                   (multiple-value-bind (c-ht c-slots)
                       (cl-cc/vm/clos::%vm-class-slots-of obj-ht)
                     (expect c-ht :to-be class-ht)
                     (expect c-slots :to-equal '(shared-slot)))))

  (it-sequential "vm-class-slots-of-routes-class-object-to-itself"
                 (let ((class-ht (make-hash-table :test #'eq)))
                   (setf (gethash :__class-slots__ class-ht) '(shared-slot)
                         (gethash 'shared-slot class-ht) 1)
                   (multiple-value-bind (c-ht c-slots)
                       (cl-cc/vm/clos::%vm-class-slots-of class-ht)
                     (expect c-ht :to-be class-ht)
                     (expect c-slots :to-equal '(shared-slot)))
                   (cl-cc/vm/clos::%vm-raw-slot-write class-ht '(shared-slot) class-ht 'shared-slot 7)
                   (expect (= 7 (cl-cc/vm/clos::%vm-raw-slot-read class-ht '(shared-slot) class-ht 'shared-slot)) :to-be-truthy)))

  (it-sequential "vm-apply-initarg-instance-slot"
                 (let* ((obj-ht      (make-hash-table :test #'eq))
                        (class-ht    (make-hash-table :test #'eq))
                        (initarg-map '((:width . width))))
                   (cl-cc/vm/clos::%vm-apply-initarg :width 100 initarg-map '() class-ht obj-ht)
                   (expect (= 100 (gethash 'width obj-ht)) :to-be-truthy)))

  (it-sequential "vm-apply-initarg-class-slot"
                 (let* ((obj-ht      (make-hash-table :test #'eq))
                        (class-ht    (make-hash-table :test #'eq))
                        (initarg-map '((:count . count))))
                   (cl-cc/vm/clos::%vm-apply-initarg :count 5 initarg-map '(count) class-ht obj-ht)
                   (expect (= 5 (gethash 'count class-ht)) :to-be-truthy)
                   (expect (gethash 'count obj-ht) :to-be-null)))

  (it-sequential "vm-slot-exists-p-declared-and-undeclared declared"
                 (destructuring-bind (slot-sym expected-p) (list 'width t)
                   (let ((s (make-clos-vm)))
                     (make-test-instance s :R0 :R1 'rect '(width height))
                     (cl-cc/vm/protocols:execute-instruction
                      (cl-cc/vm:make-vm-slot-exists-p :dst :R2 :obj-reg :R1 :slot-name-sym slot-sym)
                      s 0 nil)
                     (if expected-p
                         (expect (cl-cc/vm:vm-reg-get s :R2) :to-be-truthy)
                       (expect (cl-cc/vm:vm-reg-get s :R2) :to-be-falsy)))))

  (it-sequential "vm-slot-exists-p-declared-and-undeclared undeclared"
                 (destructuring-bind (slot-sym expected-p) (list 'color nil)
                   (let ((s (make-clos-vm)))
                     (make-test-instance s :R0 :R1 'rect '(width height))
                     (cl-cc/vm/protocols:execute-instruction
                      (cl-cc/vm:make-vm-slot-exists-p :dst :R2 :obj-reg :R1 :slot-name-sym slot-sym)
                      s 0 nil)
                     (if expected-p
                         (expect (cl-cc/vm:vm-reg-get s :R2) :to-be-truthy)
                       (expect (cl-cc/vm:vm-reg-get s :R2) :to-be-falsy)))))

  (it-sequential "vm-class-name-fn-stores-name"
                 (let ((s (make-clos-vm)))
                   (exec-class-def s :R0 'my-class)
                   (cl-cc/vm:vm-reg-set s :R1 (cl-cc/vm:vm-reg-get s :R0))
                   (cl-cc/vm/protocols:execute-instruction
                    (cl-cc/vm:make-vm-class-name-fn :dst :R2 :src :R1)
                    s 0 nil)
                   (expect (cl-cc/vm:vm-reg-get s :R2) :to-be 'my-class)))

  (it-sequential "vm-class-name-fn-advances-pc"
                 (let ((s (make-clos-vm)))
                   (exec-class-def s :R0 'pc-test)
                   (cl-cc/vm:vm-reg-set s :R1 (cl-cc/vm:vm-reg-get s :R0))
                   (let ((new-pc (first (multiple-value-list
                                         (cl-cc/vm/protocols:execute-instruction
                                          (cl-cc/vm:make-vm-class-name-fn :dst :R2 :src :R1)
                                          s 3 nil)))))
                     (expect (= 4 new-pc) :to-be-truthy))))

  (it-sequential "vm-class-of-fn-returns-class-ht"
                 (let ((s (make-clos-vm)))
                   (make-test-instance s :R0 :R1 'dog '(name breed))
                   (cl-cc/vm/protocols:execute-instruction
                    (cl-cc/vm:make-vm-class-of-fn :dst :R2 :src :R1)
                    s 0 nil)
                   (expect (cl-cc/vm:vm-reg-get s :R2) :to-be (cl-cc/vm:vm-reg-get s :R0))))

  (it-sequential "vm-find-class-registered-and-unknown registered"
                 (destructuring-bind (class-sym register-p) (list 'cat t)
                   (let ((s (make-clos-vm)))
                     (when register-p (exec-class-def s :R0 class-sym))
                     (cl-cc/vm:vm-reg-set s :R1 class-sym)
                     (cl-cc/vm/protocols:execute-instruction
                      (cl-cc/vm:make-vm-find-class :dst :R2 :src :R1)
                      s 0 nil)
                     (let ((found (cl-cc/vm:vm-reg-get s :R2)))
                       (if register-p
                           (progn
                             (expect (hash-table-p found) :to-be-truthy)
                             (expect (gethash :__name__ found) :to-be class-sym))
                         (expect found :to-be-null))))))

  (it-sequential "vm-find-class-registered-and-unknown unknown"
                 (destructuring-bind (class-sym register-p) (list 'completely-unknown-class-xyz nil)
                   (let ((s (make-clos-vm)))
                     (when register-p (exec-class-def s :R0 class-sym))
                     (cl-cc/vm:vm-reg-set s :R1 class-sym)
                     (cl-cc/vm/protocols:execute-instruction
                      (cl-cc/vm:make-vm-find-class :dst :R2 :src :R1)
                      s 0 nil)
                     (let ((found (cl-cc/vm:vm-reg-get s :R2)))
                       (if register-p
                           (progn
                             (expect (hash-table-p found) :to-be-truthy)
                             (expect (gethash :__name__ found) :to-be class-sym))
                         (expect found :to-be-null))))))
  (it-sequential "vm-defunctionalized-dispatch-resolves-known-function-tag"
  (let ((s (make-test-vm)))
    (let* ((registered (make-instance 'cl-cc/vm/execution:vm-closure-object
                                      :entry-label "fn-registered"
                                      :params nil
                                      :captured-regs #()
                                      :captured-vals #()))
           (placeholder (make-instance 'cl-cc/vm/execution:vm-closure-object
                                       :entry-label "fn-placeholder"
                                       :params nil
                                       :captured-regs #()
                                       :captured-vals #()
                                       :dispatch-tag '(:known-function . myfn))))
      (setf (gethash 'myfn (cl-cc/vm/execution:vm-function-registry s)) registered)
      (expect (cl-cc/vm/clos::%vm-defunctionalized-dispatch s placeholder) :to-be registered))))
  (it-sequential "fr-888-slot-value-by-index-reads-and-writes-in-constant-time"
                 (let* ((class (registry-add-class (make-test-registry) (quote point) :slots (quote (x y))))
                        (instance (cl-cc/vm/clos::allocate-instance-vector class))
                        (x-index (cl-cc/vm/clos::class-slot-vector-index class (quote x))))
                   (setf (cl-cc/vm/clos::slot-value-by-index instance x-index) 42)
                   (expect (= 42 (cl-cc/vm/clos::slot-value-by-index instance x-index)) :to-be-truthy)))
  (it-sequential "fr-889-zero-arg-template-copy-produces-fresh-vector"
                 (let* ((class (registry-add-class (make-test-registry) (quote point) :slots (quote (x))))
                        (first nil)
                        (second nil))
                   (setf first (cl-cc/vm/clos::%vm-build-instance-template class)
                         second (cl-cc/vm/clos::%vm-build-instance-template class))
                   (expect (eq first second) :to-be-falsy)
                   (setf (cl-cc/vm/clos::slot-value-by-index first 1) 10)
                   (expect (cl-cc/vm/clos::slot-value-by-index second 1) :to-be-null)))
  )
