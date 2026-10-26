;;;; tests/unit/vm/primitives-typep-tests.lisp — VM vm-typep Tests
;;;
;;; Tests for vm-typep, vm-typep-check, and *vm-compound-type-handlers*.
;;; Shared instruction helpers come from cl-cc/vm-test-support.

(in-package #:cl-cc/vm/execution/test)

(defun %run-vm-typep (value type-name)
  "Run vm-typep against VALUE and TYPE-NAME, returning the destination register."
  (%run-unary-inst-with
   (lambda (src)
     (cl-cc/vm/execution:make-vm-typep :dst 0 :src src :type-name type-name))
   value))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 1: vm-typep General Predicate (primitive and compound)
;;; ═══════════════════════════════════════════════════════════════════════════

(defun register-primitives-typep-tests ()
  (it-sequential "prim-typep integer"
                 (destructuring-bind (src type-sym expected) (list 42 'integer 1)
                   (expect (= expected (%run-vm-typep src type-sym)) :to-be-truthy)))

  (it-sequential "prim-typep string"
                 (destructuring-bind (src type-sym expected) (list "hello" 'string 1)
                   (expect (= expected (%run-vm-typep src type-sym)) :to-be-truthy)))

  (it-sequential "prim-typep symbol"
                 (destructuring-bind (src type-sym expected) (list 'foo 'symbol 1)
                   (expect (= expected (%run-vm-typep src type-sym)) :to-be-truthy)))

  (it-sequential "prim-typep cons"
                 (destructuring-bind (src type-sym expected) (list '(a) 'cons 1)
                   (expect (= expected (%run-vm-typep src type-sym)) :to-be-truthy)))

  (it-sequential "prim-typep null"
                 (destructuring-bind (src type-sym expected) (list nil 'null 1)
                   (expect (= expected (%run-vm-typep src type-sym)) :to-be-truthy)))

  (it-sequential "prim-typep list-cons"
                 (destructuring-bind (src type-sym expected) (list '(a) 'list 1)
                   (expect (= expected (%run-vm-typep src type-sym)) :to-be-truthy)))

  (it-sequential "prim-typep list-nil"
                 (destructuring-bind (src type-sym expected) (list nil 'list 1)
                   (expect (= expected (%run-vm-typep src type-sym)) :to-be-truthy)))

  (it-sequential "prim-typep char"
                 (destructuring-bind (src type-sym expected) (list #\a 'character 1)
                   (expect (= expected (%run-vm-typep src type-sym)) :to-be-truthy)))

  (it-sequential "prim-typep atom-num"
                 (destructuring-bind (src type-sym expected) (list 42 'atom 1)
                   (expect (= expected (%run-vm-typep src type-sym)) :to-be-truthy)))

  (it-sequential "prim-typep int-wrong"
                 (destructuring-bind (src type-sym expected) (list "hello" 'integer 0)
                   (expect (= expected (%run-vm-typep src type-sym)) :to-be-truthy)))

  (it-sequential "prim-typep refine-true"
                 (destructuring-bind (src type-sym expected) (list 42 '(cl-cc/vm/data:refine fixnum plusp) 1)
                   (expect (= expected (%run-vm-typep src type-sym)) :to-be-truthy)))

  (it-sequential "prim-typep refine-false"
                 (destructuring-bind (src type-sym expected) (list -1 '(cl-cc/vm/data:refine fixnum plusp) 0)
                   (expect (= expected (%run-vm-typep src type-sym)) :to-be-truthy)))

  (it-sequential "prim-typep or-first"
                 (destructuring-bind (src type-sym expected) (list 42 '(or integer string) 1)
                   (expect (= expected (%run-vm-typep src type-sym)) :to-be-truthy)))

  (it-sequential "prim-typep or-second"
                 (destructuring-bind (src type-sym expected) (list "hi" '(or integer string) 1)
                   (expect (= expected (%run-vm-typep src type-sym)) :to-be-truthy)))

  (it-sequential "prim-typep or-none"
                 (destructuring-bind (src type-sym expected) (list 'x '(or integer string) 0)
                   (expect (= expected (%run-vm-typep src type-sym)) :to-be-truthy)))

  (it-sequential "prim-typep and-both"
                 (destructuring-bind (src type-sym expected) (list 42 '(and integer number) 1)
                   (expect (= expected (%run-vm-typep src type-sym)) :to-be-truthy)))

  (it-sequential "prim-typep and-fail"
                 (destructuring-bind (src type-sym expected) (list 42 '(and integer string) 0)
                   (expect (= expected (%run-vm-typep src type-sym)) :to-be-truthy)))

  (it-sequential "prim-typep not-true"
                 (destructuring-bind (src type-sym expected) (list 42 '(not string) 1)
                   (expect (= expected (%run-vm-typep src type-sym)) :to-be-truthy)))

  (it-sequential "prim-typep not-false"
                 (destructuring-bind (src type-sym expected) (list "hi" '(not string) 0)
                   (expect (= expected (%run-vm-typep src type-sym)) :to-be-truthy)))

  (it-sequential "prim-typep member-hit"
                 (destructuring-bind (src type-sym expected) (list 2 '(member 1 2 3) 1)
                   (expect (= expected (%run-vm-typep src type-sym)) :to-be-truthy)))

  (it-sequential "prim-typep member-miss"
                 (destructuring-bind (src type-sym expected) (list 5 '(member 1 2 3) 0)
                   (expect (= expected (%run-vm-typep src type-sym)) :to-be-truthy)))

  (it-sequential "prim-typep eql-hit"
                 (destructuring-bind (src type-sym expected) (list 42 '(eql 42) 1)
                   (expect (= expected (%run-vm-typep src type-sym)) :to-be-truthy)))

  (it-sequential "prim-typep eql-miss"
                 (destructuring-bind (src type-sym expected) (list 99 '(eql 42) 0)
                   (expect (= expected (%run-vm-typep src type-sym)) :to-be-truthy)))

  (it-sequential "prim-typep values-any"
                 (destructuring-bind (src type-sym expected) (list 42 '(values) 1)
                   (expect (= expected (%run-vm-typep src type-sym)) :to-be-truthy)))

  (it-sequential "prim-typep function-fn"
                 (destructuring-bind (src type-sym expected) (list #'car '(function) 1)
                   (expect (= expected (%run-vm-typep src type-sym)) :to-be-truthy)))

  (it-sequential "prim-typep function-int"
                 (destructuring-bind (src type-sym expected) (list 42 '(function) 0)
                   (expect (= expected (%run-vm-typep src type-sym)) :to-be-truthy)))

  (it-sequential "prim-typep satisfies-true"
                 (destructuring-bind (src type-sym expected) (list 4 '(satisfies evenp) 1)
                   (expect (= expected (%run-vm-typep src type-sym)) :to-be-truthy)))

  (it-sequential "prim-typep satisfies-false"
                 (destructuring-bind (src type-sym expected) (list 3 '(satisfies evenp) 0)
                   (expect (= expected (%run-vm-typep src type-sym)) :to-be-truthy)))

  ;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 2: vm-typep-check and *vm-primitive-type-predicates* table
;;; ═══════════════════════════════════════════════════════════════════════════

  (it-sequential "prim-typep-check-primitive-table integer"
                 (destructuring-bind (value type-sym expected) (list 42 'integer t)
                   (expect (cl-cc/vm::vm-typep-check value type-sym) :to-equal expected)))

  (it-sequential "prim-typep-check-primitive-table string"
                 (destructuring-bind (value type-sym expected) (list "hi" 'string t)
                   (expect (cl-cc/vm::vm-typep-check value type-sym) :to-equal expected)))

  (it-sequential "prim-typep-check-primitive-table symbol"
                 (destructuring-bind (value type-sym expected) (list 'x 'symbol t)
                   (expect (cl-cc/vm::vm-typep-check value type-sym) :to-equal expected)))

  (it-sequential "prim-typep-check-primitive-table keyword"
                 (destructuring-bind (value type-sym expected) (list :k 'keyword t)
                   (expect (cl-cc/vm::vm-typep-check value type-sym) :to-equal expected)))

  (it-sequential "prim-typep-check-primitive-table cons"
                 (destructuring-bind (value type-sym expected) (list '(a) 'cons t)
                   (expect (cl-cc/vm::vm-typep-check value type-sym) :to-equal expected)))

  (it-sequential "prim-typep-check-primitive-table null"
                 (destructuring-bind (value type-sym expected) (list nil 'null t)
                   (expect (cl-cc/vm::vm-typep-check value type-sym) :to-equal expected)))

  (it-sequential "prim-typep-check-primitive-table list"
                 (destructuring-bind (value type-sym expected) (list '(1 2) 'list t)
                   (expect (cl-cc/vm::vm-typep-check value type-sym) :to-equal expected)))

  (it-sequential "prim-typep-check-primitive-table number"
                 (destructuring-bind (value type-sym expected) (list 3.14 'number t)
                   (expect (cl-cc/vm::vm-typep-check value type-sym) :to-equal expected)))

  (it-sequential "prim-typep-check-primitive-table character"
                 (destructuring-bind (value type-sym expected) (list #\a 'character t)
                   (expect (cl-cc/vm::vm-typep-check value type-sym) :to-equal expected)))

  (it-sequential "prim-typep-check-primitive-table function"
                 (destructuring-bind (value type-sym expected) (list #'car 'function t)
                   (expect (cl-cc/vm::vm-typep-check value type-sym) :to-equal expected)))

  (it-sequential "prim-typep-check-primitive-table vector"
                 (destructuring-bind (value type-sym expected) (list #(1 2) 'vector t)
                   (expect (cl-cc/vm::vm-typep-check value type-sym) :to-equal expected)))

  (it-sequential "prim-typep-check-primitive-table array"
                 (destructuring-bind (value type-sym expected) (list #(1 2) 'array t)
                   (expect (cl-cc/vm::vm-typep-check value type-sym) :to-equal expected)))

  (it-sequential "prim-typep-check-primitive-table integer-wrong"
                 (destructuring-bind (value type-sym expected) (list "hello" 'integer nil)
                   (expect (cl-cc/vm::vm-typep-check value type-sym) :to-equal expected)))

  (it-sequential "prim-typep-check-primitive-table string-wrong"
                 (destructuring-bind (value type-sym expected) (list 42 'string nil)
                   (expect (cl-cc/vm::vm-typep-check value type-sym) :to-equal expected)))

  (it-sequential "prim-typep-normalize-sym-roundtrips-cl-symbols"
                 (expect (cl-cc/vm::%vm-typep-normalize-sym 'integer) :to-be 'cl:integer)
                 (expect (cl-cc/vm::%vm-typep-normalize-sym 'string) :to-be 'cl:string)
                 (expect (cl-cc/vm::%vm-typep-normalize-sym 'symbol) :to-be 'cl:symbol))

  )
;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 1: Type Predicates (pred1 pattern — return 1/0)
;;; ═══════════════════════════════════════════════════════════════════════════

(it-sequential "prim-type-pred cons-p-true"
  (destructuring-bind (ctor src-val expected) (list #'cl-cc/vm/execution:make-vm-cons-p '(a . b) 1)
    (expect (= expected (%run-unary-inst ctor src-val)) :to-be-truthy)))
(it-sequential "prim-type-pred cons-p-false"
  (destructuring-bind (ctor src-val expected) (list #'cl-cc/vm/execution:make-vm-cons-p 42 0)
    (expect (= expected (%run-unary-inst ctor src-val)) :to-be-truthy)))
(it-sequential "prim-type-pred null-p-true"
  (destructuring-bind (ctor src-val expected) (list #'cl-cc/vm/execution:make-vm-null-p nil 1)
    (expect (= expected (%run-unary-inst ctor src-val)) :to-be-truthy)))
(it-sequential "prim-type-pred null-p-false"
  (destructuring-bind (ctor src-val expected) (list #'cl-cc/vm/execution:make-vm-null-p 'x 0)
    (expect (= expected (%run-unary-inst ctor src-val)) :to-be-truthy)))
(it-sequential "prim-type-pred symbol-p-true"
  (destructuring-bind (ctor src-val expected) (list #'cl-cc/vm/execution:make-vm-symbol-p 'hello 1)
    (expect (= expected (%run-unary-inst ctor src-val)) :to-be-truthy)))
(it-sequential "prim-type-pred symbol-p-false"
  (destructuring-bind (ctor src-val expected) (list #'cl-cc/vm/execution:make-vm-symbol-p 42 0)
    (expect (= expected (%run-unary-inst ctor src-val)) :to-be-truthy)))
;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 2: EQL comparison (pred2 — return 1/0)
;;; ═══════════════════════════════════════════════════════════════════════════

(it-sequential "prim-eq equal"
  (destructuring-bind (lhs rhs expected) (list 42 42 1)
    (expect (= expected (%run-binary-inst #'cl-cc/vm/execution:make-vm-eq lhs rhs)) :to-be-truthy)))
(it-sequential "prim-eq not-equal"
  (destructuring-bind (lhs rhs expected) (list 42 99 0)
    (expect (= expected (%run-binary-inst #'cl-cc/vm/execution:make-vm-eq lhs rhs)) :to-be-truthy)))
(it-sequential "prim-trampoline-instruction-produces-forceable-thunk"
  (let ((state (make-test-vm)))
    (cl-cc/vm/protocols:vm-reg-set state 1 20)
    (cl-cc/vm/protocols:vm-reg-set state 2 22)
    (cl-cc/vm/protocols:vm-reg-set state 9 #'+)
    (exec1 (cl-cc/vm/execution:make-vm-trampoline :dst 0 :func 9 :args '(1 2)) state)
    (let ((thunk (cl-cc/vm/protocols:vm-reg-get state 0)))
      (expect (cl-cc/vm/execution:vm-trampoline-thunk-p thunk) :to-be-truthy)
      (expect (= 42 (cl-cc/vm/execution:vm-force-trampoline-result thunk)) :to-be-truthy))))
;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 7: Boolean Operations
;;; ═══════════════════════════════════════════════════════════════════════════

(it-sequential "prim-not-nil-returns-true"
  (expect (%run-unary-inst #'cl-cc/vm/execution:make-vm-not nil) :to-equal t))
(it-sequential "prim-not-zero-returns-true"
  (expect (%run-unary-inst #'cl-cc/vm/execution:make-vm-not 0) :to-equal t))
(it-sequential "prim-not-integer-returns-nil"
  (expect (%run-unary-inst #'cl-cc/vm/execution:make-vm-not 42) :to-be-null))
(it-sequential "prim-and-cases both-true"
  (destructuring-bind (lhs rhs expected) (list 1 2 t)
    (expect (%run-binary-inst #'cl-cc/vm/execution:make-vm-and lhs rhs) :to-equal expected)))
(it-sequential "prim-and-cases zero-false"
  (destructuring-bind (lhs rhs expected) (list 0 2 nil)
    (expect (%run-binary-inst #'cl-cc/vm/execution:make-vm-and lhs rhs) :to-equal expected)))
(it-sequential "prim-and-cases one-false"
  (destructuring-bind (lhs rhs expected) (list 1 nil nil)
    (expect (%run-binary-inst #'cl-cc/vm/execution:make-vm-and lhs rhs) :to-equal expected)))
(it-sequential "prim-or-cases both-false"
  (destructuring-bind (lhs rhs expected) (list nil nil nil)
    (expect (%run-binary-inst #'cl-cc/vm/execution:make-vm-or lhs rhs) :to-equal expected)))
(it-sequential "prim-or-cases zero-false"
  (destructuring-bind (lhs rhs expected) (list nil 0 nil)
    (expect (%run-binary-inst #'cl-cc/vm/execution:make-vm-or lhs rhs) :to-equal expected)))
(it-sequential "prim-or-cases one-true"
  (destructuring-bind (lhs rhs expected) (list nil 42 t)
    (expect (%run-binary-inst #'cl-cc/vm/execution:make-vm-or lhs rhs) :to-equal expected)))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 2: vm-typep-check and *vm-primitive-type-predicates* table
;;; ═══════════════════════════════════════════════════════════════════════════

