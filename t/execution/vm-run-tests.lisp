;;;; tests/unit/vm/vm-run-tests.lisp — VM error dispatch tests

(in-package #:cl-cc/vm/execution/test)



(progn
(defun make-bytecode (&rest words)
  "Build a simple-vector bytecode from alternating opcode/dst/src1/src2 quads."
  (coerce words 'simple-vector))
(defun register-vm-run-tests ()
  (it-sequential "vm-error-type-matches string-matches-error"
                 (destructuring-bind (error-val handler-type expected-result) (list "boom" 'error t)
                   (let ((actual (cl-cc/vm/execution:vm-error-type-matches-p error-val handler-type)))
                     (if expected-result
                         (expect actual :to-be-truthy)
                       (expect actual :to-be-falsy)))))

  (it-sequential "vm-error-type-matches string-matches-condition"
                 (destructuring-bind (error-val handler-type expected-result) (list "boom" 'condition t)
                   (let ((actual (cl-cc/vm/execution:vm-error-type-matches-p error-val handler-type)))
                     (if expected-result
                         (expect actual :to-be-truthy)
                       (expect actual :to-be-falsy)))))

  (it-sequential "vm-error-type-matches string-matches-t"
                 (destructuring-bind (error-val handler-type expected-result) (list "boom" 't t)
                   (let ((actual (cl-cc/vm/execution:vm-error-type-matches-p error-val handler-type)))
                     (if expected-result
                         (expect actual :to-be-truthy)
                       (expect actual :to-be-falsy)))))

  (it-sequential "vm-error-type-matches string-no-match-specific-subtype"
                 (destructuring-bind (error-val handler-type expected-result) (list "boom" 'type-error nil)
                   (let ((actual (cl-cc/vm/execution:vm-error-type-matches-p error-val handler-type)))
                     (if expected-result
                         (expect actual :to-be-truthy)
                       (expect actual :to-be-falsy)))))

  (it-sequential "vm-error-type-matches condition-object-matches-t"
                 (destructuring-bind (error-val handler-type expected-result) (list (make-condition 'simple-error :format-control "x") 't t)
                   (let ((actual (cl-cc/vm/execution:vm-error-type-matches-p error-val handler-type)))
                     (if expected-result
                         (expect actual :to-be-truthy)
                       (expect actual :to-be-falsy)))))

  (it-sequential "build-label-table-uses-integer-keyed-buckets"
                 (let* ((instructions (list (cl-cc/vm:make-vm-label :name "entry")
                                            (cl-cc/vm:make-vm-const :dst :r0 :value 1)
                                            (cl-cc/vm:make-vm-label :name :done)
                                            (cl-cc/vm:make-vm-halt :reg :r0)))
                        (labels (cl-cc/vm/protocols:build-label-table instructions)))
                   ;; SBCL's HASH-TABLE-TEST returns the symbol 'EQL, not the function object.
                   (expect (hash-table-test labels) :to-be 'eql)
                   (expect (= 0 (cl-cc/vm/execution:vm-label-table-lookup labels "entry")) :to-be-truthy)
                   (expect (= 2 (cl-cc/vm/execution:vm-label-table-lookup labels :done)) :to-be-truthy)))

  (it-sequential "vm-jump-uses-label-table-lookup"
                 (let* ((instructions (list (cl-cc/vm:make-vm-jump :label (copy-seq "target"))
                                            (cl-cc/vm:make-vm-label :name (copy-seq "target"))
                                            (cl-cc/vm:make-vm-halt :reg :r0)))
                        (labels (cl-cc/vm/protocols:build-label-table instructions))
                        (state (make-test-vm)))
                   (multiple-value-bind (next-pc halt-p result)
                       (cl-cc/vm/protocols:execute-instruction (first instructions) state 0 labels)
                     (declare (ignore result))
                     (expect (= 1 next-pc) :to-be-truthy)
                     (expect halt-p :to-be-falsy))))

  (it-sequential "vm-label-table-prefers-identity-for-equal-names"
                 (let* ((first-label (copy-seq "target"))
                        (second-label (copy-seq "target"))
                        (instructions (list (cl-cc/vm:make-vm-jump :label first-label)
                                            (cl-cc/vm:make-vm-label :name first-label)
                                            (cl-cc/vm:make-vm-jump :label second-label)
                                            (cl-cc/vm:make-vm-label :name second-label)
                                            (cl-cc/vm:make-vm-halt :reg :r0)))
                        (labels (cl-cc/vm/protocols:build-label-table instructions)))
                   (expect (= 1 (cl-cc/vm/execution:vm-label-table-lookup labels first-label)) :to-be-truthy)
                   (expect (= 3 (cl-cc/vm/execution:vm-label-table-lookup labels second-label)) :to-be-truthy)))

;;; ─── VM2 defopcode / run-vm tests ───────────────────────────────────────────



  (it-sequential "vm2-opcode-registration"
                 (expect (numberp cl-cc/vm:+op2-const+) :to-be-truthy)
                 (expect (not (null (aref cl-cc/vm/execution:*opcode-dispatch-table* cl-cc/vm:+op2-const+))) :to-be-truthy)
                 (expect (not (null (aref cl-cc/vm/execution:*opcode-dispatch-table* cl-cc/vm:+op2-add2+))) :to-be-truthy)
                 (expect (aref cl-cc/vm/execution:*opcode-name-table* cl-cc/vm:+op2-add2+) :to-equal 'cl-cc/vm:add2)
                 (expect (= cl-cc/vm:+op2-const+ (gethash 'cl-cc/vm/execution::const cl-cc/vm/execution:*opcode-encoder-table*)) :to-be-truthy)
                 (expect (= cl-cc/vm:+op2-move+ (gethash 'cl-cc/vm:move  cl-cc/vm/execution:*opcode-encoder-table*)) :to-be-truthy)
                 (expect (= cl-cc/vm:+op2-add2+ (gethash 'cl-cc/vm:add2  cl-cc/vm/execution:*opcode-encoder-table*)) :to-be-truthy)
                 (expect (= cl-cc/vm:+op2-add-imm2+ (gethash 'cl-cc/vm:add-imm2 cl-cc/vm/execution:*opcode-encoder-table*)) :to-be-truthy))

  (it-sequential "vm2-opcode-distinct-values"
                 (let ((ops (list cl-cc/vm:+op2-const+
                                  cl-cc/vm:+op2-move+
                                  cl-cc/vm:+op2-add2+
                                  cl-cc/vm:+op2-add-imm2+
                                  cl-cc/vm:+op2-sub2+
                                  cl-cc/vm:+op2-sub-imm2+
                                  cl-cc/vm:+op2-mul2+
                                  cl-cc/vm:+op2-mul-imm2+
                                  cl-cc/vm:+op2-num-eq-imm2+
                                  cl-cc/vm:+op2-num-lt-imm2+
                                  cl-cc/vm:+op2-num-gt-imm2+
                                  cl-cc/vm:+op2-num-le-imm2+
                                  cl-cc/vm:+op2-num-ge-imm2+
                                  cl-cc/vm:+op2-halt2+)))
                   (expect (= (cl-cc/vm/data:length ops) (cl-cc/vm/data:length (remove-duplicates ops))) :to-be-truthy)))

  (it-sequential "vm2-reg-operations"
                 (let ((s (cl-cc/vm:make-vm2-state)))
                   (cl-cc/vm/protocols:vm-reg-set s 0 42)
                   (expect (= 42 (cl-cc/vm/protocols:vm-reg-get s 0)) :to-be-truthy)
                   ;; set returns the written value
                   (expect (= 99 (cl-cc/vm/protocols:vm-reg-set s 5 99)) :to-be-truthy)
                   ;; overwrite
                   (cl-cc/vm/protocols:vm-reg-set s 3 100)
                   (cl-cc/vm/protocols:vm-reg-set s 3 200)
                   (expect (= 200 (cl-cc/vm/protocols:vm-reg-get s 3)) :to-be-truthy)
                   ;; all 256 slots are independent
                   (dotimes (i 256)
                     (cl-cc/vm/protocols:vm-reg-set s i i))
                   (dotimes (i 256)
                     (expect (= i (cl-cc/vm/protocols:vm-reg-get s i)) :to-be-truthy))))

  ))
;;; ─── VM2 defopcode / run-vm tests ───────────────────────────────────────────

(it-sequential "run-vm-basic-ops nop"
  (destructuring-bind (bytecode expected)
      (list
       (make-bytecode cl-cc/vm:+op2-nop+
                      0
                      nil
                      nil
                      cl-cc/vm:+op2-const+
                      0
                      42
                      nil
                      cl-cc/vm:+op2-halt2+
                      0
                      nil
                      nil)
       42)
    (let ((s (cl-cc/vm:make-vm2-state)))
      (expect (cl-cc/vm/execution::%run-vm-core bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops load-const"
  (destructuring-bind (bytecode expected)
      (list
       (make-bytecode cl-cc/vm:+op2-load-const+
                      0
                      '(1 2 3)
                      nil
                      cl-cc/vm:+op2-halt2+
                      0
                      nil
                      nil)
       '(1 2 3))
    (let ((s (cl-cc/vm:make-vm2-state)))
      (expect (cl-cc/vm/execution::%run-vm-core bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops const-load"
  (destructuring-bind (bytecode expected)
      (list
       (make-bytecode cl-cc/vm:+op2-const+
                      0
                      42
                      nil
                      cl-cc/vm:+op2-halt2+
                      0
                      nil
                      nil)
       42)
    (let ((s (cl-cc/vm:make-vm2-state)))
      (expect (cl-cc/vm/execution::%run-vm-core bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops load-nil"
  (destructuring-bind (bytecode expected)
      (list
       (make-bytecode cl-cc/vm:+op2-load-nil+
                      0
                      nil
                      nil
                      cl-cc/vm:+op2-halt2+
                      0
                      nil
                      nil)
       nil)
    (let ((s (cl-cc/vm:make-vm2-state)))
      (expect (cl-cc/vm/execution::%run-vm-core bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops load-true"
  (destructuring-bind (bytecode expected)
      (list
       (make-bytecode cl-cc/vm:+op2-load-true+
                      0
                      nil
                      nil
                      cl-cc/vm:+op2-halt2+
                      0
                      nil
                      nil)
       t)
    (let ((s (cl-cc/vm:make-vm2-state)))
      (expect (cl-cc/vm/execution::%run-vm-core bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops load-fixnum"
  (destructuring-bind (bytecode expected)
      (list
       (make-bytecode cl-cc/vm:+op2-load-fixnum+
                      0
                      123
                      nil
                      cl-cc/vm:+op2-halt2+
                      0
                      nil
                      nil)
       123)
    (let ((s (cl-cc/vm:make-vm2-state)))
      (expect (cl-cc/vm/execution::%run-vm-core bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops move"
  (destructuring-bind (bytecode expected)
      (list
       (make-bytecode cl-cc/vm:+op2-const+
                      1
                      7
                      nil
                      cl-cc/vm:+op2-move+
                      0
                      1
                      nil
                      cl-cc/vm:+op2-halt2+
                      0
                      nil
                      nil)
       7)
    (let ((s (cl-cc/vm:make-vm2-state)))
      (expect (cl-cc/vm/execution::%run-vm-core bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops neg"
  (destructuring-bind (bytecode expected)
      (list
       (make-bytecode cl-cc/vm:+op2-const+
                      1
                      7
                      nil
                      cl-cc/vm:+op2-neg+
                      0
                      1
                      nil
                      cl-cc/vm:+op2-halt2+
                      0
                      nil
                      nil)
       -7)
    (let ((s (cl-cc/vm:make-vm2-state)))
      (expect (cl-cc/vm/execution::%run-vm-core bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops inc"
  (destructuring-bind (bytecode expected)
      (list
       (make-bytecode cl-cc/vm:+op2-const+
                      1
                      7
                      nil
                      cl-cc/vm:+op2-inc+
                      0
                      1
                      nil
                      cl-cc/vm:+op2-halt2+
                      0
                      nil
                      nil)
       8)
    (let ((s (cl-cc/vm:make-vm2-state)))
      (expect (cl-cc/vm/execution::%run-vm-core bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops dec"
  (destructuring-bind (bytecode expected)
      (list
       (make-bytecode cl-cc/vm:+op2-const+
                      1
                      7
                      nil
                      cl-cc/vm:+op2-dec+
                      0
                      1
                      nil
                      cl-cc/vm:+op2-halt2+
                      0
                      nil
                      nil)
       6)
    (let ((s (cl-cc/vm:make-vm2-state)))
      (expect (cl-cc/vm/execution::%run-vm-core bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops add"
  (destructuring-bind (bytecode expected)
      (list
       (make-bytecode cl-cc/vm:+op2-const+
                      1
                      3
                      nil
                      cl-cc/vm:+op2-const+
                      2
                      4
                      nil
                      cl-cc/vm:+op2-add2+
                      0
                      1
                      2
                      cl-cc/vm:+op2-halt2+
                      0
                      nil
                      nil)
       7)
    (let ((s (cl-cc/vm:make-vm2-state)))
      (expect (cl-cc/vm/execution::%run-vm-core bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops sub"
  (destructuring-bind (bytecode expected)
      (list
       (make-bytecode cl-cc/vm:+op2-const+
                      1
                      10
                      nil
                      cl-cc/vm:+op2-const+
                      2
                      3
                      nil
                      cl-cc/vm:+op2-sub2+
                      0
                      1
                      2
                      cl-cc/vm:+op2-halt2+
                      0
                      nil
                      nil)
       7)
    (let ((s (cl-cc/vm:make-vm2-state)))
      (expect (cl-cc/vm/execution::%run-vm-core bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops mul"
  (destructuring-bind (bytecode expected)
      (list
       (make-bytecode cl-cc/vm:+op2-const+
                      1
                      6
                      nil
                      cl-cc/vm:+op2-const+
                      2
                      7
                      nil
                      cl-cc/vm:+op2-mul2+
                      0
                      1
                      2
                      cl-cc/vm:+op2-halt2+
                      0
                      nil
                      nil)
       42)
    (let ((s (cl-cc/vm:make-vm2-state)))
      (expect (cl-cc/vm/execution::%run-vm-core bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops div"
  (destructuring-bind (bytecode expected)
      (list
       (make-bytecode cl-cc/vm:+op2-const+
                      1
                      21
                      nil
                      cl-cc/vm:+op2-const+
                      2
                      3
                      nil
                      cl-cc/vm:+op2-div+
                      0
                      1
                      2
                      cl-cc/vm:+op2-halt2+
                      0
                      nil
                      nil)
       7)
    (let ((s (cl-cc/vm:make-vm2-state)))
      (expect (cl-cc/vm/execution::%run-vm-core bytecode s) :to-equal expected))))

;;; ─── make-vm-state ──────────────────────────────────────────────────────────

;;; ─── vm-state-registers / vm-output-stream / vm-global-vars (generics) ─────

;;; ─── vm-reg-get / vm-reg-set on public VM state ─────────────────────────────

;;; ─── vm2 low-level constructor remains available explicitly ────────────────

;;; ─── run-vm-with-opcode-bigrams ─────────────────────────────────────────────

