;;;; tests/unit/vm/vm-clos-tests.lisp — CLOS infrastructure + error dispatch unit tests

(in-package #:cl-cc/vm/clos/test)

;;; Helpers — build a minimal class-registry hash table

(defun make-test-registry ()
  "Return a fresh empty registry (eq-keyed hash table)."
  (make-hash-table :test #'eq))

(defun registry-add-class (registry name &key (superclasses '()) (slots '()) (initargs '()))
  "Insert a minimal class entry into REGISTRY and return the class hash table."
  (let ((ht (make-hash-table :test #'eq)))
    (setf (gethash :__name__          ht) name
          (gethash :__superclasses__  ht) superclasses
          (gethash :__slots__         ht) slots
          (gethash :__initargs__      ht) initargs)
    (setf (gethash name registry) ht)
    ht))

;;; 1. collect-inherited-slots

;;; 2. collect-inherited-initargs

;;; 3. compute-class-precedence-list

;;; 4. EQL specializer dispatch index

;;; 5. %cis-walk (extracted DFS helper for collect-inherited-slots)

;;; 6. %cia-walk (extracted DFS helper for collect-inherited-initargs)

;;; 6b. %vm-allow-other-keys-p / %vm-validate-initargs

;;; 7. %cpl-linearize (extracted C3 linearization step)

;;; FR-821: copy operations

(defclass vm-copy-host-object ()
  ((items :initarg :items :accessor vm-copy-host-object-items)
   (flag :initarg :flag :accessor vm-copy-host-object-flag)))

;;; FR-838: extensible sequence protocol

(defclass vm-test-deque (cl-cc/vm/data:sequence)
  ((storage :initarg :storage :accessor vm-test-deque-storage)))

(defmethod cl-cc/vm/data:length ((cl-cc/vm/data:sequence vm-test-deque))
  (cl-cc/vm/data:length (vm-test-deque-storage cl-cc/vm/data:sequence)))

(defmethod cl-cc/vm/data:elt ((cl-cc/vm/data:sequence vm-test-deque) index)
  (aref (vm-test-deque-storage cl-cc/vm/data:sequence) index))

(defmethod (setf cl-cc/vm/data:elt) (value (cl-cc/vm/data:sequence vm-test-deque) index)
  (setf (aref (vm-test-deque-storage cl-cc/vm/data:sequence) index) value))

(defmethod cl-cc/vm/data:make-sequence-like ((cl-cc/vm/data:sequence vm-test-deque) size &key initial-element)
  (declare (ignore cl-cc/vm/data:sequence))
  (make-instance 'vm-test-deque
                 :storage (make-array size :initial-element initial-element)))

;;; 8. FR-888 allocate-instance fast path

;;; 9. FR-889 default-initargs / make-instance caching



(defclass vm-copy-empty-host-object () ())

(defun register-vm-clos-tests ()
  (it-sequential "collect-inherited-slots"
                 (let ((reg (make-test-registry)))
                   ;; no superclasses
                   (expect (cl-cc/vm/clos:collect-inherited-slots '() reg) :to-be-null))
                 (let ((reg (make-test-registry)))
                   ;; single superclass
                   (registry-add-class reg 'animal :slots '(name age))
                   (expect (cl-cc/vm/clos:collect-inherited-slots '(animal) reg) :to-equal '(name age)))
                 (let ((reg (make-test-registry)))
                   ;; multiple superclasses
                   (registry-add-class reg 'flyable  :slots '(wingspan))
                   (registry-add-class reg 'swimable :slots '(fin-count))
                   (let ((result (cl-cc/vm/clos:collect-inherited-slots '(flyable swimable) reg)))
                     (expect (member 'wingspan  result) :to-be-truthy)
                     (expect (member 'fin-count result) :to-be-truthy)))
                 (let ((reg (make-test-registry)))
                   ;; no duplicates
                   (registry-add-class reg 'base  :slots '(id))
                   (registry-add-class reg 'mixin :slots '(id extra))
                   (let ((result (cl-cc/vm/clos:collect-inherited-slots '(base mixin) reg)))
                     (expect (= 2 (cl-cc/vm/data:length result)) :to-be-truthy)
                     (expect (member 'id    result) :to-be-truthy)
                     (expect (member 'extra result) :to-be-truthy))))

  (it-sequential "collect-inherited-initargs"
                 (let ((reg (make-test-registry)))
                   ;; no superclasses
                   (expect (cl-cc/vm/clos:collect-inherited-initargs '() reg) :to-be-null))
                 (let ((reg (make-test-registry)))
                   ;; single superclass
                   (registry-add-class reg 'person :initargs '((:name . name) (:age . age)))
                   (let ((result (cl-cc/vm/clos:collect-inherited-initargs '(person) reg)))
                     (expect (assoc :name result) :to-be-truthy)
                     (expect (assoc :age  result) :to-be-truthy)))
                 (let ((reg (make-test-registry)))
                   ;; multiple superclasses; first occurrence wins
                   (registry-add-class reg 'named  :initargs '((:name . name)))
                   (registry-add-class reg 'tagged :initargs '((:tag . tag)))
                   (let ((result (cl-cc/vm/clos:collect-inherited-initargs '(named tagged) reg)))
                     (expect (assoc :name result) :to-be-truthy)
                     (expect (assoc :tag  result) :to-be-truthy))))

  (it-sequential "compute-class-precedence-list"
                 (let ((reg (make-test-registry)))
                   (registry-add-class reg 'root)
                   (expect (cl-cc/vm/clos:compute-class-precedence-list 'root reg) :to-equal '(root)))
                 (let ((reg (make-test-registry)))
                   (registry-add-class reg 'animal)
                   (registry-add-class reg 'dog :superclasses '(animal))
                   (expect (cl-cc/vm/clos:compute-class-precedence-list 'dog reg) :to-equal '(dog animal)))
                 (let ((reg (make-test-registry)))
                   (registry-add-class reg 'c)
                   (registry-add-class reg 'b :superclasses '(c))
                   (registry-add-class reg 'a :superclasses '(b))
                   (expect (cl-cc/vm/clos:compute-class-precedence-list 'a reg) :to-equal '(a b c)))
                 (let ((reg (make-test-registry)))
                   ;; diamond: A -> B, A -> C, B -> D, C -> D
                   (registry-add-class reg 'd)
                   (registry-add-class reg 'b :superclasses '(d))
                   (registry-add-class reg 'c :superclasses '(d))
                   (registry-add-class reg 'a :superclasses '(b c))
                   (let ((cpl (cl-cc/vm/clos:compute-class-precedence-list 'a reg)))
                     (expect (member 'a cpl) :to-be-truthy)
                     (expect (member 'b cpl) :to-be-truthy)
                     (expect (member 'c cpl) :to-be-truthy)
                     (expect (member 'd cpl) :to-be-truthy)
                     (expect (= (cl-cc/vm/data:length cpl) (cl-cc/vm/data:length (remove-duplicates cpl))) :to-be-truthy)
                     (expect cpl :to-equal '(a b c d))))
                 (let ((reg (make-test-registry)))
                   (registry-add-class reg 'a)
                   (registry-add-class reg 'b :superclasses '(a))
                   (registry-add-class reg 'c :superclasses '(a))
                   (registry-add-class reg 'd :superclasses '(b c))
                   (expect (cl-cc/vm/clos:compute-class-precedence-list 'd reg) :to-equal '(d b c a)))
                 (let ((reg (make-test-registry)))
                   (registry-add-class reg 'a)
                   (registry-add-class reg 'b)
                   (registry-add-class reg 'x :superclasses '(a b))
                   (registry-add-class reg 'y :superclasses '(b a))
                   (expect (cl-cc/vm/clos:compute-class-precedence-list 'x reg) :to-equal '(x a b))
                   (expect (cl-cc/vm/clos:compute-class-precedence-list 'y reg) :to-equal '(y b a)))
                 (let ((reg (make-test-registry)))
                   (registry-add-class reg 'z)
                   (registry-add-class reg 'y :superclasses '(z))
                   (registry-add-class reg 'x :superclasses '(z))
                   (registry-add-class reg 'w :superclasses '(y x))
                   (expect (cl-cc/vm/clos:compute-class-precedence-list 'w reg) :to-equal '(w y x z))))

  (it-sequential "eql-specializer-dispatch-index"
                 (let* ((state (make-test-vm))
                        (gf (make-hash-table :test #'equal))
                        (method 'read-method)
                        (inst (cl-cc/vm:make-vm-register-method
                               :gf-reg :r0
                               :specializer '(eql :read)
                               :qualifier nil
                               :method-reg :r1)))
                   (setf (gethash :__methods__ gf) (make-hash-table :test #'equal)
                         (gethash :__eql-index__ gf) (make-hash-table :test #'equal))
                   (cl-cc/vm:vm-reg-set state :r0 gf)
                   (cl-cc/vm:vm-reg-set state :r1 method)
                   (cl-cc/vm/protocols:execute-instruction inst state 0 nil)
                   (expect (cl-cc/vm/clos:%vm-gf-eql-methods gf :read) :to-equal (list method))
                   (expect (gethash :read (gethash :__eql-index__ gf)) :to-equal (list method))))

  (it-sequential "cis-walk-empty-class-list-leaves-result-unchanged"
                 (let ((reg (make-test-registry))
                       (seen (make-hash-table :test #'eq))
                       (cell (list nil)))
                   (cl-cc/vm/clos::%cis-walk '() reg seen cell)
                   (expect (car cell) :to-be-null)))

  (it-sequential "cis-walk-single-class-accumulates-slots"
                 (let ((reg (make-test-registry))
                       (seen (make-hash-table :test #'eq))
                       (cell (list nil)))
                   (registry-add-class reg 'thing :slots '(x y))
                   (cl-cc/vm/clos::%cis-walk '(thing) reg seen cell)
                   (expect (member 'x (car cell)) :to-be-truthy)
                   (expect (member 'y (car cell)) :to-be-truthy)))

  (it-sequential "cis-walk-deduplicates-slots-via-seen"
                 (let ((reg (make-test-registry))
                       (seen (make-hash-table :test #'eq))
                       (cell (list nil)))
                   (setf (gethash 'id seen) t)
                   (registry-add-class reg 'base :slots '(id extra))
                   (cl-cc/vm/clos::%cis-walk '(base) reg seen cell)
                   (expect (member 'id    (car cell)) :to-be-falsy)
                   (expect (member 'extra (car cell)) :to-be-truthy)))

  (it-sequential "cia-walk-empty-class-list-leaves-result-unchanged"
                 (let ((reg (make-test-registry))
                       (cell (list nil)))
                   (cl-cc/vm/clos::%cia-walk '() reg cell)
                   (expect (car cell) :to-be-null)))

  (it-sequential "cia-walk-single-class-accumulates-initargs"
                 (let ((reg (make-test-registry))
                       (cell (list nil)))
                   (registry-add-class reg 'named :initargs '((:name . name)))
                   (cl-cc/vm/clos::%cia-walk '(named) reg cell)
                   (expect (assoc :name (car cell)) :to-be-truthy)))

  (it-sequential "cia-walk-does-not-duplicate-existing-keys"
                 (let ((reg (make-test-registry))
                       (cell (list (list '(:name . name-old)))))
                   (registry-add-class reg 'alt :initargs '((:name . name-new) (:extra . extra)))
                   (cl-cc/vm/clos::%cia-walk '(alt) reg cell)
                   (expect (= 1 (cl-cc/vm/data:length (remove-if-not (lambda (e) (eq (car e) :name)) (car cell)))) :to-be-truthy)
                   (expect (assoc :extra (car cell)) :to-be-truthy)))

  (it-sequential "vm-allow-other-keys-p-cases truthy"
                 (destructuring-bind (reg-val alist) (list t '((:allow-other-keys . :r0)))
                   (let ((s (make-test-vm)))
                     (cl-cc/vm:vm-reg-set s :r0 reg-val)
                     (if reg-val
                         (expect (cl-cc/vm/clos::%vm-allow-other-keys-p alist s) :to-be-truthy)
                       (expect (cl-cc/vm/clos::%vm-allow-other-keys-p alist s) :to-be-falsy)))))

  (it-sequential "vm-allow-other-keys-p-cases nil-val"
                 (destructuring-bind (reg-val alist) (list nil '((:allow-other-keys . :r0)))
                   (let ((s (make-test-vm)))
                     (cl-cc/vm:vm-reg-set s :r0 reg-val)
                     (if reg-val
                         (expect (cl-cc/vm/clos::%vm-allow-other-keys-p alist s) :to-be-truthy)
                       (expect (cl-cc/vm/clos::%vm-allow-other-keys-p alist s) :to-be-falsy)))))

  (it-sequential "vm-allow-other-keys-p-cases absent"
                 (destructuring-bind (reg-val alist) (list nil '((:x . :r0)))
                   (let ((s (make-test-vm)))
                     (cl-cc/vm:vm-reg-set s :r0 reg-val)
                     (if reg-val
                         (expect (cl-cc/vm/clos::%vm-allow-other-keys-p alist s) :to-be-truthy)
                       (expect (cl-cc/vm/clos::%vm-allow-other-keys-p alist s) :to-be-falsy)))))

  (it-sequential "vm-validate-initargs-known-key-accepts-without-signal"
                 (let ((s (make-test-vm)))
                   (cl-cc/vm:vm-reg-set s :r0 42)
                   (expect (progn
                             (cl-cc/vm/clos::%vm-validate-initargs '((:width . :r0)) '((:width . width)) s)
                             t) :to-be-truthy)))

  (it-sequential "vm-validate-initargs-unknown-key-signals-error"
                 (let ((s (make-test-vm)))
                   (cl-cc/vm:vm-reg-set s :r0 42)
                   (signals error (cl-cc/vm/clos::%vm-validate-initargs '((:unknown-key . :r0)) '((:width . width)) s))))

  (it-sequential "vm-validate-initargs-allow-other-keys-bypasses-check"
                 (let ((s (make-test-vm)))
                   (cl-cc/vm:vm-reg-set s :r0 42)
                   (cl-cc/vm:vm-reg-set s :r1 t)
                   (expect (progn
                             (cl-cc/vm/clos::%vm-validate-initargs
                              '((:unknown-key . :r0) (:allow-other-keys . :r1))
                              '((:width . width))
                              s)
                             t) :to-be-truthy)))

  (it-sequential "cpl-linearize-unknown-class-returns-singleton"
                 (let ((reg (make-test-registry)))
                   (expect (cl-cc/vm/clos::%cpl-linearize 'unknown reg) :to-equal '(unknown))))

  (it-sequential "cpl-linearize-leaf-class-returns-singleton"
                 (let ((reg (make-test-registry)))
                   (registry-add-class reg 'leaf)
                   (expect (cl-cc/vm/clos::%cpl-linearize 'leaf reg) :to-equal '(leaf))))

  (it-sequential "cpl-linearize-linear-inheritance-chain"
                 (let ((reg (make-test-registry)))
                   (registry-add-class reg 'c)
                   (registry-add-class reg 'b :superclasses '(c))
                   (registry-add-class reg 'a :superclasses '(b))
                   (expect (cl-cc/vm/clos::%cpl-linearize 'a reg) :to-equal '(a b c))))

  (it-sequential "fr-821-copy-instance-host-standard-object"
                 (let* ((items (list 1 2 3))
                        (object (make-instance 'vm-copy-host-object :items items :flag :original))
                        (copy (cl-cc/vm/clos:copy-instance object)))
                   (setf (vm-copy-host-object-flag copy) :changed)
                   (expect (eq object copy) :to-be-falsy)
                   (expect (vm-copy-host-object-items copy) :to-be items)
                   (expect (vm-copy-host-object-flag object) :to-be :original)
                   (expect (vm-copy-host-object-flag copy) :to-be :changed)))
  (it-sequential "mop-copy-instance-distinct-object"
                 (let ((state (make-clos-vm)))
                   (make-test-instance state :R0 :R1 'mop-copy-distinct '(x))
                   (let* ((object (cl-cc/vm:vm-reg-get state :R1))
                          (copy (cl-cc/vm/clos:copy-instance object)))
                     (expect (eq object copy) :to-be-falsy))))
  (it-sequential "mop-copy-instance-same-class"
                 (let ((state (make-clos-vm)))
                   (make-test-instance state :R0 :R1 'mop-copy-class '(x))
                   (let* ((object (cl-cc/vm:vm-reg-get state :R1))
                          (copy (cl-cc/vm/clos:copy-instance object)))
                     (expect (eq (test-instance-class object)
                                 (test-instance-class copy))
                             :to-be-truthy))))
  (it-sequential "mop-copy-instance-shallow-slot-copy"
                 (let* ((state (make-clos-vm))
                        (items (list 1 2 3)))
                   (make-test-instance state :R0 :R1 'mop-copy-shallow '(items))
                   (cl-cc/vm:vm-reg-set state :R2 items)
                   (cl-cc/vm/protocols:execute-instruction
                    (cl-cc/vm/clos:make-vm-slot-write
                     :obj-reg :R1 :slot-name 'items :value-reg :R2)
                    state 0 nil)
                   (let* ((object (cl-cc/vm:vm-reg-get state :R1))
                          (copy (cl-cc/vm/clos:copy-instance object)))
                     (cl-cc/vm:vm-reg-set state :R3 copy)
                     (cl-cc/vm/protocols:execute-instruction
                      (cl-cc/vm/clos:make-vm-slot-read
                       :dst :R4 :obj-reg :R3 :slot-name 'items)
                      state 0 nil)
                     (expect (eq (cl-cc/vm:vm-reg-get state :R4) items)
                             :to-be-truthy))))
  (it-sequential "fr-821-deep-copy-recursive-conses-and-vectors"
                 (let* ((nested (vector (list :a :b)))
                        (copy (cl-cc/vm/clos:deep-copy nested)))
                   (setf (car (aref nested 0)) :changed)
                   (expect (eq nested copy) :to-be-falsy)
                   (expect (eq (aref nested 0) (aref copy 0)) :to-be-falsy)
                   (expect (car (aref copy 0)) :to-be :a)))

  (it-sequential "fr-838-extensible-sequence-subseq-and-setf-elt"
                 (let* ((deque (make-instance 'vm-test-deque :storage (vector 10 20 30 40)))
                        (slice (cl-cc/vm/data:subseq deque 1 3)))
                   (setf (cl-cc/vm/data:elt deque 2) 99)
                   (expect (cl-cc/vm/data:sequence-protocol-p deque) :to-be-truthy)
                   (expect (= 4 (cl-cc/vm/data:length deque)) :to-be-truthy)
                   (expect (= 99 (cl-cc/vm/data:elt deque 2)) :to-be-truthy)
                   (expect (= 2 (cl-cc/vm/data:length slice)) :to-be-truthy)
                   (expect (= 20 (cl-cc/vm/data:elt slice 0)) :to-be-truthy)
                   (expect (= 30 (cl-cc/vm/data:elt slice 1)) :to-be-truthy)))

  (it-sequential "fr-888-dynamic-layout-falls-back-from-vector-allocation"
                 (let ((class (registry-add-class (make-test-registry) 'dynamic :slots '(x))))
                   (setf (gethash :__dynamic-slot-layout__ class) t)
                   (signals error (cl-cc/vm/clos::allocate-instance-vector class))))
  (it-sequential "copy-instance-hash-table-shallow-copy"
                 (let* ((original (make-hash-table :test #'equal))
                        (copy nil))
                   (setf (gethash :__class__ original) :my-class
                         (gethash :slot-a original) 42)
                   (setf copy (cl-cc/vm/clos:copy-instance original))
                   (expect (eq original copy) :to-be-falsy)
                   (expect (gethash :__class__ copy) :to-be :my-class)
                   (expect (= 42 (gethash :slot-a copy)) :to-be-truthy)
                   (setf (gethash :slot-a original) 999)
                   (expect (= 42 (gethash :slot-a copy)) :to-be-truthy)))
  (it-sequential "copy-instance-missing-class-key-signals-error"
                 (let ((bare (make-hash-table)))
                   (setf (gethash :slot-x bare) 1)
                   (signals error (cl-cc/vm/clos:copy-instance bare))))
  (it-sequential "copy-instance-vector-with-hash-header-uses-copy-seq"
                 (let* ((header (make-hash-table))
                        (original (vector header 10 20))
                        (copy (cl-cc/vm/clos:copy-instance original)))
                   (expect (vectorp copy) :to-be-truthy)
                   (expect (eq original copy) :to-be-falsy)
                   (expect (= 10 (cl:aref copy 1)) :to-be-truthy)))
  (it-sequential "copy-instance-standard-object-is-shallow"
                 (let* ((object (make-instance 'vm-copy-empty-host-object))
                        (copy nil))
                   (setf copy (cl-cc/vm/clos:copy-instance object))
                   (expect (eq object copy) :to-be-falsy)
                   (expect (typep copy 'vm-copy-empty-host-object) :to-be-truthy)))
  (it-sequential "matches symbols and SETF names without matching malformed names"
    (let ((left (make-symbol "SLOT-VALUE-USING-CLASS"))
          (right (make-symbol "SLOT-VALUE-USING-CLASS")))
      (expect (cl-cc/vm/clos::%vm-same-function-name-p left right) :to-be-truthy)
      (expect (cl-cc/vm/clos::%vm-same-function-name-p (list 'setf left) (list 'setf right))
              :to-be-truthy)
      (expect (cl-cc/vm/clos::%vm-same-function-name-p (list 'setf left) (list 'other right))
              :to-be nil)
      (expect (cl-cc/vm/clos::%vm-same-function-name-p (list 'setf left 'extra) (list 'setf right))
              :to-be nil)
      (expect (cl-cc/vm/clos::%vm-same-function-name-p (cons 'setf left) (list 'setf right))
              :to-be nil)))

  (it-sequential "selects same-named SETF generic functions by argument precedence"
  (let* ((state (cl-cc/vm:make-vm-state))
         (lookup-name (list 'setf (make-symbol "SLOT-VALUE-USING-CLASS")))
         (broad-name (list 'setf (make-symbol "SLOT-VALUE-USING-CLASS")))
         (specific-name (list 'setf
                              (intern "SLOT-VALUE-USING-CLASS"
                                      (find-package :cl-cc))))
         (eql-name (list 'setf (make-symbol "SLOT-VALUE-USING-CLASS")))
         (tie-name (list 'setf
                         (intern "SLOT-VALUE-USING-CLASS"
                                 (find-package :cl-cc/vm/clos/test))))
         (exact-gf (make-hash-table :test #'eq))
         (broad-gf (make-hash-table :test #'eq))
         (specific-gf (make-hash-table :test #'eq))
         (eql-gf (make-hash-table :test #'eq))
         (tie-gf (make-hash-table :test #'eq))
         (broad-methods (make-hash-table :test #'equal))
         (specific-methods (make-hash-table :test #'equal))
         (eql-methods (make-hash-table :test #'equal))
         (tie-methods (make-hash-table :test #'equal))
         (broad-method (make-hash-table :test #'eq))
         (specific-method (make-hash-table :test #'eq))
         (eql-method (make-hash-table :test #'eq))
         (tie-method (make-hash-table :test #'eq))
         (metaclass (make-hash-table :test #'eq))
         (class (make-hash-table :test #'eq))
         (args nil))
    (setf (gethash :__generic__ exact-gf) t
          (gethash :__methods__ exact-gf) (make-hash-table :test #'equal)
          (gethash :__generic__ broad-gf) t
          (gethash :__methods__ broad-gf) broad-methods
          (gethash :__generic__ specific-gf) t
          (gethash :__methods__ specific-gf) specific-methods
          (gethash :__generic__ eql-gf) t
          (gethash :__methods__ eql-gf) eql-methods
          (gethash :__generic__ tie-gf) t
          (gethash :__methods__ tie-gf) tie-methods
          (gethash :specializer broad-method) '(t t t t)
          (gethash '(t t t t) broad-methods) broad-method
          (gethash :specializer specific-method) '(t mop-hook-meta t t)
          (gethash '(t mop-hook-meta t t) specific-methods) specific-method
          (gethash :specializer eql-method) '(t t (eql :object) t)
          (gethash '(t t (eql :object) t) eql-methods) eql-method
          (gethash :specializer tie-method) '(t mop-hook-meta t t)
          (gethash '(t mop-hook-meta t t) tie-methods) tie-method
          (gethash :__name__ metaclass) 'mop-hook-meta
          (gethash :__name__ class) 'mop-hook-target
          (gethash :__metaclass__ class) metaclass
          (gethash lookup-name (cl-cc/vm:vm-global-vars state)) exact-gf
          (gethash broad-name (cl-cc/vm:vm-global-vars state)) broad-gf
          (gethash specific-name (cl-cc/vm:vm-global-vars state)) specific-gf
          (gethash eql-name (cl-cc/vm:vm-global-vars state)) eql-gf
          (gethash tie-name (cl-cc/vm:vm-global-vars state)) tie-gf)
    (setf args (list :written class :object 'slot-name))
    (expect (cl-cc/vm/clos::%vm-global-generic-function state lookup-name args)
            :to-be specific-gf)
    (expect (cl-cc/vm/clos::%vm-global-generic-function state lookup-name)
            :to-be exact-gf)
    (expect (cl-cc/vm:vm-generic-function-p class)
            :to-be nil)))
  (it-sequential "selects a same-named SETF generic function with a raw EQL-keyed method"
  (let* ((state (cl-cc/vm:make-vm-state))
         (lookup-name
           (list (quote setf) (make-symbol "SLOT-VALUE-USING-CLASS")))
         (broad-name
           (list (quote setf)
                 (intern "SLOT-VALUE-USING-CLASS"
                         (find-package :cl-cc/vm/clos/test))))
         (raw-name
           (list (quote setf)
                 (intern "SLOT-VALUE-USING-CLASS"
                         (find-package :cl-cc))))
         (broad-gf (make-hash-table :test (function eq)))
         (raw-gf (make-hash-table :test (function eq)))
         (broad-methods (make-hash-table :test (function equal)))
         (raw-methods (make-hash-table :test (function equal)))
         (broad-method (make-hash-table :test (function eq))))
    (setf (gethash :__generic__ broad-gf) t
          (gethash :__methods__ broad-gf) broad-methods
          (gethash :specializer broad-method) (list t t t t)
          (gethash (list t t t t) broad-methods) broad-method
          (gethash :__generic__ raw-gf) t
          (gethash :__methods__ raw-gf) raw-methods
          (gethash (list (list (quote eql) :written) t t t) raw-methods)
          (function identity)
          (gethash broad-name (cl-cc/vm:vm-global-vars state)) broad-gf
          (gethash raw-name (cl-cc/vm:vm-global-vars state)) raw-gf)
    (expect (cl-cc/vm/clos::%vm-global-generic-function
             state lookup-name (list :written :class :object :slot))
            :to-be raw-gf)))
  (it-sequential "terminal around call-next-method reflects available continuation"
    (let* ((state (cl-cc/vm:make-vm-state))
           (gf (make-hash-table :test #'eq))
           (primary-methods (make-hash-table :test #'equal))
           (around-methods (make-hash-table :test #'equal))
           (method (make-hash-table :test #'eq))
           (closure
             (let ((labels (make-hash-table :test #'eql)))
               (cl-cc/vm:vm-label-table-store labels :around 0)
               (make-instance 'cl-cc/vm/execution:vm-closure-object
                              :entry-label :around
                              :params '(:arg)
                              :program-flat
                              (vector (cl-cc/vm:make-vm-next-method-p :dst :HAS-NEXT)
                                      (cl-cc/vm:make-vm-call-next-method :dst :RESULT)
                                      (cl-cc/vm:make-vm-ret :reg :RESULT))
                              :label-table labels))))
      (setf (gethash :__generic__ gf) t
            (gethash :__methods__ gf) primary-methods
            (gethash :__AROUND__ gf) around-methods
            (gethash :function method) closure
            (gethash :specializer method) t
            (gethash t around-methods) method)
      (expect (cl-cc/vm/clos::%vm-call-generic-sync
               gf state (list :arg) :default (lambda () :fallback))
              :to-be :fallback)
      (expect (cl-cc/vm:vm-reg-get state :HAS-NEXT) :to-be-truthy)
      (signals error
        (cl-cc/vm/clos::%vm-call-generic-sync gf state (list :arg)))
      (signals error
        (let ((no-primary-gf (make-hash-table :test #'eq)))
          (setf (gethash :__generic__ no-primary-gf) t
                (gethash :__methods__ no-primary-gf) (make-hash-table :test #'equal)
                (gethash :__AROUND__ no-primary-gf) (make-hash-table :test #'equal))
          (cl-cc/vm/clos::%vm-call-generic-sync no-primary-gf state (list :arg))))
      (expect (cl-cc/vm:vm-reg-get state :HAS-NEXT) :to-be-falsy)
      (expect (cl-cc/vm:vm-call-stack state) :to-be-null)
      (expect (cl-cc/vm:vm-method-call-stack state) :to-be-null)))
  (it-sequential "rejects call-next-method arguments that change applicability"
    (let* ((state (cl-cc/vm:make-vm-state))
           (gf (make-hash-table :test #'eq))
           (methods (make-hash-table :test #'equal))
           (specific-method (make-hash-table :test #'eq))
           (symbol-method (make-hash-table :test #'eq))
           (general-method (make-hash-table :test #'eq))
           (specific-closure
             (let ((labels (make-hash-table :test #'eql)))
               (cl-cc/vm:vm-label-table-store labels :specific 0)
               (make-instance 'cl-cc/vm/execution:vm-closure-object
                              :entry-label :specific
                              :params '(:arg)
                              :program-flat
                              (vector (cl-cc/vm:make-vm-const
                                       :dst :NEXT-ARGS :value (list "changed"))
                                      (cl-cc/vm:make-vm-call-next-method
                                       :dst :RESULT :args-reg :NEXT-ARGS)
                                      (cl-cc/vm:make-vm-ret :reg :RESULT))
                              :label-table labels)))
           (symbol-closure
             (let ((labels (make-hash-table :test #'eql)))
               (cl-cc/vm:vm-label-table-store labels :symbol 0)
               (make-instance 'cl-cc/vm/execution:vm-closure-object
                              :entry-label :symbol
                              :params '(:arg)
                              :program-flat
                              (vector (cl-cc/vm:make-vm-const
                                       :dst :RESULT :value :symbol)
                                      (cl-cc/vm:make-vm-ret :reg :RESULT))
                              :label-table labels)))
           (general-closure
             (let ((labels (make-hash-table :test #'eql)))
               (cl-cc/vm:vm-label-table-store labels :general 0)
               (make-instance 'cl-cc/vm/execution:vm-closure-object
                              :entry-label :general
                              :params '(:arg)
                              :program-flat
                              (vector (cl-cc/vm:make-vm-const
                                       :dst :RESULT :value :general)
                                      (cl-cc/vm:make-vm-ret :reg :RESULT))
                              :label-table labels))))
      (setf (gethash :__generic__ gf) t
            (gethash :__methods__ gf) methods
            (gethash :function specific-method) specific-closure
            (gethash :specializer specific-method) '(eql :initial)
            (gethash '(eql :initial) methods) specific-method
            (gethash :function symbol-method) symbol-closure
            (gethash :specializer symbol-method) 'symbol
            (gethash 'symbol methods) symbol-method
            (gethash :function general-method) general-closure
            (gethash :specializer general-method) t
            (gethash t methods) general-method)
      (signals error
        (cl-cc/vm/clos::%vm-call-generic-sync gf state (list :initial)))
      (expect (cl-cc/vm:vm-call-stack state) :to-be-null)
      (expect (cl-cc/vm:vm-method-call-stack state) :to-be-null)))
  (it-sequential "rejects call-next-method arguments that change applicable method order"
    (let* ((state (cl-cc/vm:make-vm-state))
           (registry (cl-cc/vm:vm-class-registry state))
           (class-a (registry-add-class registry 'rank-a))
           (class-b (registry-add-class registry 'rank-b))
           (class-x (registry-add-class registry 'rank-x))
           (class-y (registry-add-class registry 'rank-y))
           (object-x (make-hash-table :test #'eq))
           (object-y (make-hash-table :test #'eq))
           (args-x (list object-x :token))
           (args-y (list object-y :token))
           (gf (make-hash-table :test #'eq))
           (methods (make-hash-table :test #'equal))
           (method-a (make-hash-table :test #'eq))
           (method-a-tie (make-hash-table :test #'eq))
           (method-b (make-hash-table :test #'eq))
           (closure-a
             (let ((labels (make-hash-table :test #'eql)))
               (cl-cc/vm:vm-label-table-store labels :a 0)
               (make-instance 'cl-cc/vm/execution:vm-closure-object
                              :entry-label :a
                              :params '(:class :value)
                              :program-flat
                              (vector (cl-cc/vm:make-vm-const
                                       :dst :NEXT-ARGS :value args-y)
                                      (cl-cc/vm:make-vm-call-next-method
                                       :dst :RESULT :args-reg :NEXT-ARGS)
                                      (cl-cc/vm:make-vm-ret :reg :RESULT))
                              :label-table labels)))
           (closure-a-tie
             (let ((labels (make-hash-table :test #'eql)))
               (cl-cc/vm:vm-label-table-store labels :a-tie 0)
               (make-instance 'cl-cc/vm/execution:vm-closure-object
                              :entry-label :a-tie
                              :params '(:class :value)
                              :program-flat
                              (vector (cl-cc/vm:make-vm-const
                                       :dst :RESULT :value :same-specializers)
                                      (cl-cc/vm:make-vm-ret :reg :RESULT))
                              :label-table labels)))
           (closure-b
             (let ((labels (make-hash-table :test #'eql)))
               (cl-cc/vm:vm-label-table-store labels :b 0)
               (make-instance 'cl-cc/vm/execution:vm-closure-object
                              :entry-label :b
                              :params '(:class :value)
                              :program-flat
                              (vector (cl-cc/vm:make-vm-const
                                       :dst :RESULT :value :rank-b)
                                      (cl-cc/vm:make-vm-ret :reg :RESULT))
                              :label-table labels))))
      (setf (gethash :__cpl__ class-a) '(rank-a t)
            (gethash :__cpl__ class-b) '(rank-b t)
            (gethash :__cpl__ class-x) '(rank-x rank-a rank-b t)
            (gethash :__cpl__ class-y) '(rank-y rank-b rank-a t)
            (gethash :__class__ object-x) class-x
            (gethash :__class__ object-y) class-y
            (gethash :__generic__ gf) t
            (gethash :__methods__ gf) methods
            (gethash :function method-a) closure-a
            (gethash :specializers method-a) '(rank-a (eql :token))
            (gethash :function method-a-tie) closure-a-tie
            (gethash :specializers method-a-tie) '(rank-a (eql :token))
            (gethash :function method-b) closure-b
            (gethash :specializers method-b) '(rank-b t)
            (gethash '(rank-a (eql :token)) methods) (list method-a method-a-tie)
            (gethash '(rank-b t) methods) method-b)
      (expect (mapcar #'cdr
                      (cl-cc/vm/clos::%vm-collect-applicable-method-entries
                       methods state args-x))
              :to-equal (list method-a method-a-tie method-b))
      (expect (mapcar #'cdr
                      (cl-cc/vm/clos::%vm-collect-applicable-method-entries
                       methods state args-y))
              :to-equal (list method-b method-a method-a-tie))
      (signals error
        (cl-cc/vm/clos::%vm-call-generic-sync gf state args-x))
      (expect (cl-cc/vm:vm-call-stack state) :to-be-null)
      (expect (cl-cc/vm:vm-method-call-stack state) :to-be-null)))
  (it-sequential "retains raw methods registered under distinct applicable keys"
    (let* ((state (cl-cc/vm:make-vm-state))
           (registry (cl-cc/vm:vm-class-registry state))
           (class (registry-add-class registry (quote raw-key-class)))
           (object (make-hash-table :test (function eq)))
           (methods (make-hash-table :test (function equal)))
           (raw-function (function identity)))
      (setf (gethash :__cpl__ class) (quote (raw-key-class t))
            (gethash :__class__ object) class
            (gethash (quote raw-key-class) methods) raw-function
            (gethash t methods) raw-function)
      (let ((entries
              (cl-cc/vm/clos::%vm-collect-applicable-method-entries
               methods state (list object))))
        (expect (mapcar (function car) entries)
                :to-equal (quote (raw-key-class t)))
        (expect (every (lambda (entry)
                         (eq (cdr entry) raw-function))
                       entries)
                :to-be-truthy))))
  (describe-sequential
    "allocate-instance direct method detection"
  (it
  "recognizes scalar and list primary allocation keys owned by the generic function"
  (let* ((gf (make-hash-table :test (function eq)))
         (methods (make-hash-table :test (function equal)))
         (method (make-hash-table :test (function eq)))
         (metaclass (quote allocation-metaclass)))
    (setf (gethash :__generic__ gf) t
          (gethash :__methods__ gf) methods
          (gethash :function method) (function identity)
          (gethash :qualifiers method) nil
          (gethash :specializer method) (list metaclass t t)
          (gethash :gf method) gf
          (gethash (list metaclass t t) methods) method)
    (expect (cl-cc/vm/clos::%vm-direct-primary-method-p gf metaclass)
            :to-be-truthy)
    (remhash (list metaclass t t) methods)
    (setf (gethash :specializer method) metaclass
          (gethash metaclass methods) method)
    (expect (cl-cc/vm/clos::%vm-direct-primary-method-p gf metaclass)
            :to-be-truthy)
    (remhash metaclass methods)
    (setf (gethash (quote other-specializer) methods) method)
    (expect (cl-cc/vm/clos::%vm-direct-primary-method-p gf metaclass)
            :to-be nil)
    (clrhash methods)
    (setf (gethash :specializer method) (list metaclass t t)
          (gethash (list metaclass (quote non-top) t) methods) method)
    (expect (cl-cc/vm/clos::%vm-direct-primary-method-p gf metaclass)
            :to-be nil)
    (clrhash methods)
    (setf (gethash :qualifiers method) (list :before)
          (gethash (list metaclass t) methods) method)
    (expect (cl-cc/vm/clos::%vm-direct-primary-method-p gf metaclass)
            :to-be nil)
    (setf (gethash :qualifiers method) nil
          (gethash :gf method) (make-hash-table :test (function eq)))
    (expect (cl-cc/vm/clos::%vm-direct-primary-method-p gf metaclass)
            :to-be nil)))
  (it
      "orders scalar rest fallbacks without leaking into fixed-arity dispatch"
    (let ((gf (make-hash-table :test (function equal)))
          (methods (make-hash-table :test (function equal)))
          (before (make-hash-table :test (function equal)))
          (after (make-hash-table :test (function equal)))
          (state (cl-cc/vm:make-vm-state))
          (args (list 42 :x :value)))
      (setf (gethash :__generic__ gf) t
            (gethash :__lambda-list__ gf) (list (quote object) (quote &rest) (quote initargs))
            (gethash :__methods__ gf) methods
            (gethash :__before__ gf) before
            (gethash :__after__ gf) after
            (gethash (list (quote integer) t t) methods) (quote specific)
            (gethash t methods) (quote fallback)
            (gethash (list (quote integer) t t) before) (quote before-specific)
            (gethash t before) (quote before-fallback)
            (gethash (list (quote integer) t t) after) (quote after-specific)
            (gethash t after) (quote after-fallback))
      (expect (cl-cc/vm/clos::vm-get-all-applicable-methods gf state args)
              :to-equal (list (quote specific) (quote fallback)))
      (expect (cl-cc/vm/clos::%lookup-qualified-methods gf :__before__ state args)
              :to-equal (list (quote before-specific) (quote before-fallback)))
      (expect (cl-cc/vm/clos::%lookup-qualified-methods gf :__after__ state args)
              :to-equal (list (quote after-specific) (quote after-fallback)))
      (setf (gethash t methods) (quote specific))
      (expect (cl-cc/vm/clos::vm-get-all-applicable-methods gf state args)
              :to-equal (list (quote specific)))
      (setf (gethash :__lambda-list__ gf)
            (list (quote first) (quote second) (quote third))
            (gethash t methods) (quote fallback))
      (expect (cl-cc/vm/clos::vm-get-all-applicable-methods gf state args)
              :to-equal (list (quote specific)))))))
;;; 8. FR-888 allocate-instance fast path

;;; 9. FR-889 default-initargs / make-instance caching
;;; 8. FR-888 allocate-instance fast path

;;;