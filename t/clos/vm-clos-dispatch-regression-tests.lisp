(in-package #:cl-cc/vm/clos/test)

;;; 8. FR-888 allocate-instance fast path

;;; 9. FR-889 default-initargs / make-instance caching
;;; 8. FR-888 allocate-instance fast path

;;;

(defun register-vm-clos-regression-setf-symbol-names () (it-sequential "matches symbols and SETF names without matching malformed names"
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
              :to-be nil))))

(defun register-vm-clos-regression-setf-argument-precedence () (it-sequential "selects same-named SETF generic functions by argument precedence"
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
            :to-be nil))))

(defun register-vm-clos-regression-setf-eql-method () (it-sequential "selects a same-named SETF generic function with a raw EQL-keyed method"
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
            :to-be raw-gf))))

(defun register-vm-clos-regression-around-next-method () (it-sequential "terminal around call-next-method reflects available continuation"
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
      (expect (cl-cc/vm:vm-method-call-stack state) :to-be-null))))

(defun register-vm-clos-regression-next-method-applicability () (it-sequential "rejects call-next-method arguments that change applicability"
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
      (expect (cl-cc/vm:vm-method-call-stack state) :to-be-null))))

(defun register-vm-clos-regression-next-method-order () (it-sequential "rejects call-next-method arguments that change applicable method order"
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
      (expect (cl-cc/vm:vm-method-call-stack state) :to-be-null))))

(defun register-vm-clos-regression-raw-method-keys () (it-sequential "retains raw methods registered under distinct applicable keys"
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
                :to-be-truthy)))))
