;;;; t/vm-numeric-bignum-test.lisp — native VM bignum arithmetic
;;;;
;;;; src/vm-numeric-bignum.lisp's limb-based vm-bignum has no direct test
;;;; coverage elsewhere in this suite (only indirectly, through "runs a
;;;; compiled program end to end"). These pin its arithmetic against host
;;;; CL bignums across the full signed range, not just hand-picked cases.

(in-package :cl-cc-vm/test)

(describe-sequential "native VM bignum arithmetic"
  (it-property "round-trips through vm-integer->bignum and vm-bignum-to-integer"
      ((n (gen-integer :min -1000000000000000000000 :max 1000000000000000000000)))
    (expect (cl-cc/vm::vm-bignum-to-integer (cl-cc/vm::vm-integer->bignum n)) :to-be n))

  (it-property "adds the same as host arithmetic"
      ((a (gen-integer :min -1000000000000000000000 :max 1000000000000000000000))
       (b (gen-integer :min -1000000000000000000000 :max 1000000000000000000000)))
    (expect (cl-cc/vm::vm-bignum-to-integer
             (cl-cc/vm::vm-bignum-add (cl-cc/vm::vm-integer->bignum a)
                                       (cl-cc/vm::vm-integer->bignum b)))
            :to-be (+ a b)))

  (it-property "subtracts the same as host arithmetic"
      ((a (gen-integer :min -1000000000000000000000 :max 1000000000000000000000))
       (b (gen-integer :min -1000000000000000000000 :max 1000000000000000000000)))
    (expect (cl-cc/vm::vm-bignum-to-integer
             (cl-cc/vm::vm-bignum-sub (cl-cc/vm::vm-integer->bignum a)
                                       (cl-cc/vm::vm-integer->bignum b)))
            :to-be (- a b)))

  (it-property "multiplies the same as host arithmetic"
      ((a (gen-integer :min -100000000000 :max 100000000000))
       (b (gen-integer :min -100000000000 :max 100000000000)))
    (let* ((product (cl-cc/vm::vm-bignum-mul (cl-cc/vm::vm-integer->bignum a)
                                              (cl-cc/vm::vm-integer->bignum b))))
      (expect (cl-cc/vm::vm-bignum-to-integer product) :to-be (* a b))))

  (it-property "negation matches host negation"
      ((n (gen-integer :min -1000000000000000000000 :max 1000000000000000000000)))
    (expect (cl-cc/vm::vm-bignum-to-integer (cl-cc/vm::vm-bignum-negate
                                              (cl-cc/vm::vm-integer->bignum n)))
            :to-be (- n)))

  (it-property "gcd matches host gcd for positive operands"
      ((a (gen-integer :min 1 :max 1000000000000))
       (b (gen-integer :min 1 :max 1000000000000)))
    (expect (cl-cc/vm::vm-bignum-to-integer
             (cl-cc/vm::vm-bignum-gcd (cl-cc/vm::vm-integer->bignum a)
                                       (cl-cc/vm::vm-integer->bignum b)))
            :to-be (gcd a b))))
