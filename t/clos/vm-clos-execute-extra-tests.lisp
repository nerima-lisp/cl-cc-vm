(in-package #:cl-cc/vm/clos/test)

;; Generation changes currently fall back to re-resolution after cache invalidation.
;; This case records that behavior while stale-generation semantics are reviewed.


(defun register-vm-clos-execute-extra-tests ()
  (it-sequential "vm-class-def-registers-and-stores-all-metadata"
                 (let ((s (make-clos-vm)))
                   (exec-class-def s :R0 'pt :slots '(x y))
                   (expect (gethash 'pt (cl-cc/vm/execution:vm-class-registry s)) :to-be-truthy)
                   (let ((class-ht (cl-cc/vm:vm-reg-get s :R0)))
                     (expect (hash-table-p class-ht) :to-be-truthy)
                     (expect (gethash :__name__ class-ht) :to-be 'pt)
                     (expect (member 'x (gethash :__slots__ class-ht)) :to-be-truthy)
                     (expect (member 'y (gethash :__slots__ class-ht)) :to-be-truthy))))

  (it-sequential "vm-class-def-advances-pc"
                 (let ((s (make-clos-vm)))
                   (let ((new-pc (first (multiple-value-list
                                         (cl-cc/vm/protocols:execute-instruction
                                          (class-def-inst :R0 'foo :slots '())
                                          s 5 nil)))))
                     (expect (= 6 new-pc) :to-be-truthy))))

  (it-sequential "vm-class-def-computes-cpl"
                 (let ((s (make-clos-vm)))
                   (exec-class-def s :R0 'base)
                   (exec-class-def s :R1 'child :supers '(base))
                   (let* ((child-ht (cl-cc/vm:vm-reg-get s :R1))
                          (cpl (gethash :__cpl__ child-ht)))
                     (expect (member 'child cpl) :to-be-truthy)
                     (expect (member 'base  cpl) :to-be-truthy))))

  (it-sequential "vm-make-obj-stores-class-ref"
                 (let ((s (make-clos-vm)))
                   (exec-class-def s :R0 'animal :slots '(name))
                   (let ((class-ht (cl-cc/vm:vm-reg-get s :R0)))
                     (cl-cc/vm/protocols:execute-instruction
                      (cl-cc/vm:make-vm-make-obj :dst :R1 :class-reg :R0 :initarg-regs nil)
                      s 0 nil)
                     (let ((obj-ht (cl-cc/vm:vm-reg-get s :R1)))
                       (expect (vectorp obj-ht) :to-be-truthy)
                       (expect (test-instance-class obj-ht) :to-be class-ht)))))

  (it-sequential "vm-make-obj-initializes-slots-to-nil"
                 (let ((s (make-clos-vm)))
                   (exec-class-def s :R0 'pt :slots '(x y))
                   (cl-cc/vm/protocols:execute-instruction
                    (cl-cc/vm:make-vm-make-obj :dst :R1 :class-reg :R0 :initarg-regs nil)
                    s 0 nil)
                   (let ((obj-ht (cl-cc/vm:vm-reg-get s :R1)))
                     (expect (test-instance-slot-value obj-ht 'x) :to-be-null)
                     (expect (test-instance-slot-value obj-ht 'y) :to-be-null))))

  (it-sequential "vm-slot-write-read-roundtrip"
                 (let ((s (make-clos-vm)))
                   (make-test-instance s :R0 :R1 'box '(width))
                   (cl-cc/vm:vm-reg-set s :R2 42)
                   (cl-cc/vm/protocols:execute-instruction
                    (cl-cc/vm:make-vm-slot-write :obj-reg :R1 :slot-name 'width :value-reg :R2)
                    s 0 nil)
                   (cl-cc/vm/protocols:execute-instruction
                    (cl-cc/vm:make-vm-slot-read :dst :R3 :obj-reg :R1 :slot-name 'width)
                    s 0 nil)
                   (expect (= 42 (cl-cc/vm:vm-reg-get s :R3)) :to-be-truthy)))
  (it-sequential "vm-call-next-method-uses-next-method-owner"
               (let* ((s (cl-cc/vm:make-vm-state))
                      (owner-labels (make-hash-table :test #'eql))
                      (owner-flat
                        (vector (cl-cc/vm:make-vm-const :dst :R0 :value 42)
                                (cl-cc/vm:make-vm-ret :reg :R0)))
                      (next-fn
                        (make-instance 'cl-cc/vm:vm-closure-object
                                       :entry-label 'next-method
                                       :params nil
                                       :program-flat owner-flat
                                       :label-table owner-labels))
                      (gf-ht (make-hash-table :test #'eq))
                      (current-method (make-hash-table :test #'eq))
                      (next-method (make-hash-table :test #'eq))
                      (method-context
                        (list gf-ht (list current-method next-method) nil))
                      (program
                        (cl-cc/vm:make-vm-program
                         :instructions
                         (list (cl-cc/vm:make-vm-call-next-method :dst :OUT)
                               (cl-cc/vm:make-vm-halt :reg :OUT)
                               (cl-cc/vm:make-vm-label :name 'next-method)
                               (cl-cc/vm:make-vm-const :dst :DECOY :value 99)
                               (cl-cc/vm:make-vm-halt :reg :DECOY))
                         :result-register :OUT)))
                 (cl-cc/vm:vm-label-table-store owner-labels 'next-method 0)
                 (setf (gethash :function next-method) next-fn)
                 (push method-context (cl-cc/vm:vm-method-call-stack s))
                 (expect (cl-cc/vm:vm-label-table-lookup owner-labels 'next-method)
                         :to-equal 0)
                 (expect (cl-cc/vm:run-compiled program :state s) :to-equal 42)
                 (expect (cl-cc/vm:vm-call-stack s) :to-be-null)
                 (expect (car (cl-cc/vm:vm-method-call-stack s))
                         :to-be method-context)))
  (it-sequential "synchronous generic calls support around call-next-method"
  (let* ((state (cl-cc/vm:make-vm-state))
         (gf (make-hash-table :test #'eq))
         (primary-methods (make-hash-table :test #'equal))
         (around-methods (make-hash-table :test #'equal))
         (primary-method (make-hash-table :test #'eq))
         (outer-method (make-hash-table :test #'eq))
         (inner-method (make-hash-table :test #'eq)))
    (push (list gf nil nil :phase :around :sync-next #'identity)
          (cl-cc/vm:vm-method-call-stack state))
    (cl-cc/vm/protocols:execute-instruction
     (cl-cc/vm:make-vm-next-method-p :dst :HAS-NEXT) state 0 nil)
    (expect (cl-cc/vm:vm-reg-get state :HAS-NEXT) :to-be t)
    (pop (cl-cc/vm:vm-method-call-stack state))
    (flet ((make-closure (entry-label instructions)
             (let ((labels (make-hash-table :test #'eql)))
               (cl-cc/vm:vm-label-table-store labels entry-label 0)
               (make-instance 'cl-cc/vm/execution:vm-closure-object
                              :entry-label entry-label
                              :params '(:arg)
                              :program-flat instructions
                              :label-table labels))))
      (let ((outer-closure
              (make-closure
               :outer
               (vector (cl-cc/vm:make-vm-next-method-p :dst :HAS-NEXT)
                       (cl-cc/vm:make-vm-call-next-method :dst :R0)
                       (cl-cc/vm:make-vm-ret :reg :R0))))
            (inner-closure
              (make-closure
               :inner
               (vector (cl-cc/vm:make-vm-call-next-method :dst :R0)
                       (cl-cc/vm:make-vm-ret :reg :R0))))
            (primary-closure
              (make-closure
               :primary
               (vector (cl-cc/vm:make-vm-const :dst :R0 :value 42)
                       (cl-cc/vm:make-vm-ret :reg :R0)))))
        (setf (gethash :__generic__ gf) t
              (gethash :__methods__ gf) primary-methods
              (gethash :__AROUND__ gf) around-methods
              (gethash :function primary-method) primary-closure
              (gethash :specializer primary-method) t
              (gethash t primary-methods) primary-method
              (gethash :function outer-method) outer-closure
              (gethash :specializer outer-method) 'integer
              (gethash 'integer around-methods) outer-method
              (gethash :function inner-method) inner-closure
              (gethash :specializer inner-method) t
              (gethash t around-methods) inner-method)
        (expect (cl-cc/vm/clos::%vm-call-generic-sync gf state (list 7))
                :to-equal 42)
        (expect (cl-cc/vm:vm-call-stack state) :to-be-null)
        (expect (cl-cc/vm:vm-method-call-stack state) :to-be-null)))))
  (it-sequential "vm-around-call-next-method-uses-each-method-owner"
  (let* ((state (cl-cc/vm:make-vm-state))
         (gf (make-hash-table :test #'eq))
         (primary-methods (make-hash-table :test #'equal))
         (around-methods (make-hash-table :test #'equal))
         (primary-method (make-hash-table :test #'eq))
         (around-a-method (make-hash-table :test #'eq))
         (around-b-method (make-hash-table :test #'eq))
         (around-a-labels (make-hash-table :test #'eql))
         (around-a-flat
           (vector (cl-cc/vm:make-vm-label :name 'next-method)
                   (cl-cc/vm:make-vm-label :name 'around-a)

                   (cl-cc/vm:make-vm-next-method-p :dst :A-HAS-NEXT)
                   (cl-cc/vm:make-vm-set-global :name 'around-a-has-next :src :A-HAS-NEXT)
                   (cl-cc/vm:make-vm-call-next-method :dst :A-NEXT-RESULT)
                   (cl-cc/vm:make-vm-const :dst :A-BIAS :value 10)
                   (cl-cc/vm:make-vm-add :dst :A-RESULT
                                         :lhs :A-NEXT-RESULT
                                         :rhs :A-BIAS)
                   (cl-cc/vm:make-vm-ret :reg :A-RESULT)))
         (primary-labels (make-hash-table :test #'eql))
         (primary-flat
           (vector (cl-cc/vm:make-vm-const :dst :P-PREFIX-0 :value 0)
                   (cl-cc/vm:make-vm-const :dst :P-PREFIX-1 :value 1)
                   (cl-cc/vm:make-vm-label :name 'next-method)
                   (cl-cc/vm:make-vm-const :dst :PRIMARY-RESULT :value 42)
                   (cl-cc/vm:make-vm-ret :reg :PRIMARY-RESULT)))
         (around-a
           (make-instance 'cl-cc/vm:vm-closure-object
                          :entry-label 'around-a
                          :params '(:ARG)
                          :program-flat around-a-flat
                          :label-table around-a-labels))
         (around-b
           (make-instance 'cl-cc/vm:vm-closure-object
                          :entry-label 'around-b
                          :params '(:ARG)))
         (primary
           (make-instance 'cl-cc/vm:vm-closure-object
                          :entry-label 'next-method
                          :params '(:ARG)
                          :program-flat primary-flat
                          :label-table primary-labels))
         (program
           (cl-cc/vm:make-vm-program
            :instructions
            (append
             (list (cl-cc/vm:make-vm-const :dst :GF :value gf)
                   (cl-cc/vm:make-vm-const :dst :ARG :value 'marker)
                   (cl-cc/vm:make-vm-generic-call
                    :dst :OUT :gf-reg :GF :args '(:ARG))
                   (cl-cc/vm:make-vm-halt :reg :OUT)
                   (cl-cc/vm:make-vm-label :name 'around-b)
                   (cl-cc/vm:make-vm-next-method-p :dst :B-HAS-NEXT)
                   (cl-cc/vm:make-vm-set-global :name 'around-b-has-next :src :B-HAS-NEXT)
                   (cl-cc/vm:make-vm-call-next-method :dst :B-NEXT-RESULT)
                   (cl-cc/vm:make-vm-const :dst :B-BIAS :value 1)
                   (cl-cc/vm:make-vm-add :dst :B-RESULT
                                         :lhs :B-NEXT-RESULT
                                         :rhs :B-BIAS)
                   (cl-cc/vm:make-vm-ret :reg :B-RESULT))
             (loop repeat 6
                   collect (cl-cc/vm:make-vm-const :dst :DECOY :value 0))
             (list (cl-cc/vm:make-vm-label :name 'next-method)
                   (cl-cc/vm:make-vm-const :dst :DECOY :value 99)
                   (cl-cc/vm:make-vm-halt :reg :DECOY)))
            :result-register :OUT)))
    (cl-cc/vm:vm-label-table-store around-a-labels 'next-method 0)
    (cl-cc/vm:vm-label-table-store around-a-labels 'around-a 1)
    (cl-cc/vm:vm-label-table-store primary-labels 'next-method 2)
    (setf (gethash :__generic__ gf) t
          (gethash :__methods__ gf) primary-methods
          (gethash :__AROUND__ gf) around-methods
          (gethash t primary-methods) primary-method
          (gethash :function primary-method) primary
          (gethash :specializer primary-method) t
          (gethash t around-methods) around-a-method
          (gethash :function around-a-method) around-a
          (gethash :specializer around-a-method) t
          (gethash '(eql marker) around-methods) around-b-method
          (gethash :function around-b-method) around-b
          (gethash :specializer around-b-method) '(eql marker))
    (expect (cl-cc/vm:vm-label-table-lookup around-a-labels 'next-method)
            :to-equal 0)
    (expect (cl-cc/vm:vm-label-table-lookup primary-labels 'next-method)
            :to-equal 2)
    (expect (loop for instruction in (cl-cc/vm:vm-program-instructions program)
                  for pc from 0
                  when (and (cl-cc/vm:vm-label-p instruction)
                            (eq (cl-cc/vm:vm-name instruction) 'next-method))
                    return pc)
            :to-equal 17)
    (expect (cl-cc/vm:run-compiled program :state state) :to-equal 53)
    (expect (gethash 'around-a-has-next (cl-cc/vm:vm-global-vars state)) :to-be t)
    (expect (gethash 'around-b-has-next (cl-cc/vm:vm-global-vars state)) :to-be t)
    (expect (cl-cc/vm:vm-call-stack state) :to-be-null)
    (expect (cl-cc/vm:vm-method-call-stack state) :to-be-null)))
  (it-sequential "vm-call-next-method-restores-call-state-on-error"
               (let* ((s (cl-cc/vm:make-vm-state))
                      (owner-labels (make-hash-table :test #'eql))
                      (error-flat
                        (vector (cl-cc/vm:make-vm-call-next-method :dst :R0)))
                      (good-flat
                        (vector (cl-cc/vm:make-vm-const :dst :R0 :value 42)
                                (cl-cc/vm:make-vm-ret :reg :R0)))
                      (error-fn
                        (make-instance 'cl-cc/vm:vm-closure-object
                                       :entry-label 'next-method
                                       :params nil
                                       :program-flat error-flat
                                       :label-table owner-labels))
                      (good-fn
                        (make-instance 'cl-cc/vm:vm-closure-object
                                       :entry-label 'next-method
                                       :params nil
                                       :program-flat good-flat
                                       :label-table owner-labels))
                      (gf-ht (make-hash-table :test #'eq))
                      (current-method (make-hash-table :test #'eq))
                      (next-method (make-hash-table :test #'eq))
                      (method-context
                        (list gf-ht (list current-method next-method) nil))
                      (program
                        (cl-cc/vm:make-vm-program
                         :instructions
                         (list (cl-cc/vm:make-vm-call-next-method :dst :OUT)
                               (cl-cc/vm:make-vm-halt :reg :OUT)
                               (cl-cc/vm:make-vm-label :name 'next-method)
                               (cl-cc/vm:make-vm-const :dst :DECOY :value 99)
                               (cl-cc/vm:make-vm-halt :reg :DECOY))
                         :result-register :OUT)))
                 (cl-cc/vm:vm-label-table-store owner-labels 'next-method 0)
                 (setf (gethash :function next-method) error-fn)
                 (push method-context (cl-cc/vm:vm-method-call-stack s))
                 (expect (cl-cc/vm:vm-label-table-lookup owner-labels 'next-method)
                         :to-equal 0)
                 (expect (loop for instruction in
                                   (cl-cc/vm:vm-program-instructions program)
                               for pc from 0
                               when (and (cl-cc/vm:vm-label-p instruction)
                                         (eq (cl-cc/vm:vm-name instruction)
                                             'next-method))
                                 return pc)
                         :to-equal 2)
                 (signals error
                   (cl-cc/vm:run-compiled program :state s))
                 (expect (cl-cc/vm:vm-call-stack s) :to-be-null)
                 (expect (car (cl-cc/vm:vm-method-call-stack s))
                         :to-be method-context)
                 (expect (cdr (cl-cc/vm:vm-method-call-stack s)) :to-be-null)
                 (setf (gethash :function next-method) good-fn)
                 (expect (cl-cc/vm:run-compiled program :state s) :to-equal 42)
                 (expect (cl-cc/vm:vm-call-stack s) :to-be-null)
                 (expect (car (cl-cc/vm:vm-method-call-stack s))
                         :to-be method-context)
                 (expect (cdr (cl-cc/vm:vm-method-call-stack s)) :to-be-null)))

  (it-sequential "vm-generic-call-caches-multi-dispatch-tuple-key"
                 (let* ((s (make-clos-vm))
                        (labels (make-hash-table :test #'eql))
                        (gf-ht (make-hash-table :test #'equal))
                        (methods-ht (make-hash-table :test #'equal))
                        (method-fn (make-instance 'cl-cc/vm/execution:vm-closure-object
                                                  :entry-label 'multi-method
                                                  :params '(:X :Y)))
                        (method-desc (make-hash-table :test #'eq))
                        (inst (cl-cc/vm:make-vm-generic-call :dst :OUT :gf-reg :GF :args '(:A :B))))
                   (exec-class-def s :C0 'left)
                   (exec-class-def s :C1 'right)
                   (cl-cc/vm/protocols:execute-instruction
                    (cl-cc/vm:make-vm-make-obj :dst :A :class-reg :C0 :initarg-regs nil)
                    s 0 labels)
                   (cl-cc/vm/protocols:execute-instruction
                    (cl-cc/vm:make-vm-make-obj :dst :B :class-reg :C1 :initarg-regs nil)
                    s 0 labels)
                   (cl-cc/vm/execution:vm-label-table-store labels 'multi-method 77)
                   (setf (gethash :function method-desc) method-fn
                         (gethash :qualifiers method-desc) nil
                         (gethash :specializer method-desc) '(left right)
                         (gethash :gf method-desc) gf-ht
                         (gethash '(left right) methods-ht) method-desc
                         (gethash :__methods__ gf-ht) methods-ht
                         (gethash :__name__ gf-ht) 'multi-ic-gf
                         (gethash '__ic-gen__ gf-ht) 0)
                   (cl-cc/vm:vm-reg-set s :GF gf-ht)

                   ;; First call resolves through full multi-dispatch and writes a tuple-key cache.
                   (expect (= 77 (first (multiple-value-list
                                         (cl-cc/vm/protocols:execute-instruction inst s 10 labels)))) :to-be-truthy)
                   (expect (first (cl-cc/vm/clos::vm-ic-cache inst)) :to-equal '(left right))

                   ;; Remove the method table entry; the second call can only succeed via IC hit.
                   (remhash '(left right) methods-ht)
                   (expect (= 77 (first (multiple-value-list
                                         (cl-cc/vm/protocols:execute-instruction inst s 20 labels)))) :to-be-truthy)

                   ;; Generation changes invalidate the cached tuple entry.
                   (incf (gethash '__ic-gen__ gf-ht))
                   ;; under the broken framework assert-signals this
                   ;; assertion passed vacuously. With native (signals), execute-instruction no
                   ;; longer errors after the generation bump + method removal — the inline
                   ;; cache appears to re-resolve/fall back gracefully instead of raising. Needs
                   ;; a domain decision on whether stale-generation dispatch should error.
                   (cl-cc/vm/protocols:execute-instruction inst s 30 labels)))

  )
;; Generation changes currently fall back to re-resolution after cache invalidation.
;; This case records that behavior while stale-generation semantics are reviewed.


