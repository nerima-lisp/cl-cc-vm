(in-package #:cl-cc/vm/data/test)

(defun register-vm-execution-moved-tests ()
  (it-sequential "vm-execute-cow-array-make-and-adjust"
                   (let ((s (make-test-vm)))
                     (cl-cc/vm:vm-reg-set s :R1 3)
                     (cl-cc/vm:vm-reg-set s :R2 9)
                     (exec1 (cl-cc/vm:make-vm-make-array :dst :R0 :size-reg :R1 :initial-element :R2
                                                      :adjustable t :copy-on-write t) s)
                     (expect (cl-cc/vm/data::vm-cow-vector-p (cl-cc/vm:vm-reg-get s :R0)) :to-be-truthy)
                     (cl-cc/vm:vm-reg-set s :R3 5)
                     (exec1 (cl-cc/vm:make-vm-adjust-array :dst :R4 :arr :R0 :dims :R3 :initial-element nil) s)
                     (expect (array-total-size (cl-cc/vm:vm-reg-get s :R4)) :to-equal 5)))

  (it-sequential "prim-compound-type-handlers-table-has-expected-keys"
                   (let ((ht cl-cc/vm/data::*vm-compound-type-handlers*))
                     (expect (gethash 'or        ht) :to-be-truthy)
                     (expect (gethash 'and       ht) :to-be-truthy)
                     (expect (gethash 'not       ht) :to-be-truthy)
                     (expect (gethash 'member    ht) :to-be-truthy)
                     (expect (gethash 'eql       ht) :to-be-truthy)
                     (expect (gethash 'satisfies ht) :to-be-truthy)
                     (expect (gethash 'values    ht) :to-be-truthy)
                     (expect (gethash 'function  ht) :to-be-truthy)))

  (it-sequential "fr-787-string-builder-exists"
                   :timeout
                   5
                   (expect (fboundp 'cl-cc/vm/data:make-string-builder) :to-be-truthy)
                   (expect (fboundp 'cl-cc/vm/data:string-builder-append!) :to-be-truthy)
                   (expect (fboundp 'cl-cc/vm/data:string-builder-finish) :to-be-truthy))

  (it-sequential "fr-788-rope-exists"
                   :timeout
                   5
                   (expect (fboundp 'cl-cc/vm/data:rope) :to-be-truthy)
                   (expect (fboundp 'cl-cc/vm/data:rope-concat) :to-be-truthy)
                   (expect (fboundp 'cl-cc/vm/data:rope-split) :to-be-truthy)
                   (expect (fboundp 'cl-cc/vm/data:rope-to-string) :to-be-truthy))

  (it-sequential "fr-function-exists fr-821-copy-structure"
                   (destructuring-bind (sym) (list 'cl-cc/vm/data::copy-structure)
                     (expect (fboundp sym) :to-be-truthy)))

  (it-sequential "fr-function-exists fr-824-transients"
                   (destructuring-bind (sym) (list 'cl-cc/vm/data:transient)
                     (expect (fboundp sym) :to-be-truthy)))

  (it-sequential "fr-function-exists fr-830-taint"
                   (destructuring-bind (sym) (list 'cl-cc/vm/data::taint-mark)
                     (expect (fboundp sym) :to-be-truthy)))

  (it-sequential "fr-function-exists fr-838-sequence-proto"
                   (destructuring-bind (sym) (list 'cl-cc/vm/data:sequence-protocol-p)
                     (expect (fboundp sym) :to-be-truthy)))

  (it-sequential "fr-function-exists fr-872-cow-string"
                   (destructuring-bind (sym) (list 'cl-cc/vm/data::vm-string-copy)
                     (expect (fboundp sym) :to-be-truthy)))

  (it-sequential "fr-function-exists fr-881-rehash"
                   (destructuring-bind (sym) (list 'cl:hash-table-rehash-size)
                     (expect (fboundp sym) :to-be-truthy)))
  (it-sequential "fr-function-exists fr-873-cow-array"
                 (destructuring-bind (sym) (list 'cl-cc/vm/data::vm-cow-vector-p)
                   (expect (fboundp sym) :to-be-truthy)))
)
