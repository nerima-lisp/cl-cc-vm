;;;; tests/unit/vm/list-alist-tests.lisp — VM Alist, Copy, Predicate, and PC Tests
;;;
;;; Tests for vm-assoc, vm-acons, vm-equal, vm-nconc, vm-copy-list/tree,
;;; vm-subst, vm-listp, vm-atom, and PC advancement for list instructions.

(in-package #:cl-cc/vm/data/test)

(defun %prepare-vm-list-predicate-input (state value)
  (if (eq value :cow-list)
      (progn
        (cl-cc/vm:vm-reg-set state 2 '(a b))
        (exec1 (cl-cc/vm:make-vm-copy-list :dst 1 :src 2) state))
      (cl-cc/vm:vm-reg-set state 1 value)))

;;; ─── Association lists ──────────────────────────────────────────────────────

(defun register-list-alist-tests ()
(it-sequential "vm-list-assoc-hit-miss hit"
  (destructuring-bind (key alist expected) (list 'b '((a . 1) (b . 2) (c . 3)) '(b . 2))
    (let ((s (make-test-vm)))
    (cl-cc/vm:vm-reg-set s 1 key)
    (cl-cc/vm:vm-reg-set s 2 alist)
    (exec1 (cl-cc/vm:make-vm-assoc :dst 0 :key 1 :alist 2) s)
    (expect (cl-cc/vm:vm-reg-get s 0) :to-equal expected))))
(it-sequential "vm-list-assoc-hit-miss miss"
  (destructuring-bind (key alist expected) (list 'z '((a . 1)) nil)
    (let ((s (make-test-vm)))
    (cl-cc/vm:vm-reg-set s 1 key)
    (cl-cc/vm:vm-reg-set s 2 alist)
    (exec1 (cl-cc/vm:make-vm-assoc :dst 0 :key 1 :alist 2) s)
    (expect (cl-cc/vm:vm-reg-get s 0) :to-equal expected))))
(it-sequential "vm-list-acons-prepends-pair"
  (let ((s (make-test-vm)))
    (cl-cc/vm:vm-reg-set s 1 'x)
    (cl-cc/vm:vm-reg-set s 2 99)
    (cl-cc/vm:vm-reg-set s 3 '((a . 1)))
    (exec1 (cl-cc/vm/data:make-vm-acons :dst 0 :key 1 :value 2 :alist 3) s)
    (let ((result (cl-cc/vm:vm-reg-get s 0)))
      (expect (first result) :to-equal '(x . 99))
      (expect (= 2 (length result)) :to-be-truthy))))
(it-sequential "vm-list-assoc-on-copy-list-alist"
  (let ((s (make-test-vm)))
    (cl-cc/vm:vm-reg-set s 1 'b)
    (cl-cc/vm:vm-reg-set s 2 '((a . 1) (b . 2) (c . 3)))
    (exec1 (cl-cc/vm:make-vm-copy-list :dst 3 :src 2) s)
    (exec1 (cl-cc/vm:make-vm-assoc :dst 0 :key 1 :alist 3) s)
    (expect (cl-cc/vm:vm-reg-get s 0) :to-equal '(b . 2))))
(it-sequential "vm-list-equal-cases equal"
  (destructuring-bind (lhs rhs expected) (list '(a (b c)) '(a (b c)) 1)
    (let ((s (make-test-vm)))
    (cl-cc/vm:vm-reg-set s 1 lhs)
    (cl-cc/vm:vm-reg-set s 2 rhs)
    (exec1 (cl-cc/vm:make-vm-equal :dst 0 :lhs 1 :rhs 2) s)
    (expect (= expected (cl-cc/vm:vm-reg-get s 0)) :to-be-truthy))))
(it-sequential "vm-list-equal-cases not-equal"
  (destructuring-bind (lhs rhs expected) (list '(a b) '(a c) 0)
    (let ((s (make-test-vm)))
    (cl-cc/vm:vm-reg-set s 1 lhs)
    (cl-cc/vm:vm-reg-set s 2 rhs)
    (exec1 (cl-cc/vm:make-vm-equal :dst 0 :lhs 1 :rhs 2) s)
    (expect (= expected (cl-cc/vm:vm-reg-get s 0)) :to-be-truthy))))
(it-sequential "vm-list-nconc-joins"
  (let ((s (make-test-vm)))
    (cl-cc/vm:vm-reg-set s 1 (list 'a 'b))
    (cl-cc/vm:vm-reg-set s 2 (list 'c 'd))
    (exec1 (cl-cc/vm:make-vm-nconc :dst 0 :lhs 1 :rhs 2) s)
    (expect (cl-cc/vm:vm-reg-get s 0) :to-equal '(a b c d))))
(it-sequential "vm-list-copy-ops copy-list"
  (destructuring-bind (constructor orig deep-p) (list #'cl-cc/vm:make-vm-copy-list (list 1 2 3) nil)
    (let ((s (make-test-vm)))
    (cl-cc/vm:vm-reg-set s 1 orig)
    (exec1 (funcall constructor :dst 0 :src 1) s)
    (let ((copy (cl-cc/vm:vm-reg-get s 0)))
      (expect copy :to-equal orig)
      (when deep-p
        (expect (not (eq (first orig) (first copy))) :to-be-truthy))))))
(it-sequential "vm-list-copy-ops copy-tree"
  (destructuring-bind (constructor orig deep-p) (list #'cl-cc/vm:make-vm-copy-tree (list (list 1 2) (list 3 4)) t)
    (let ((s (make-test-vm)))
    (cl-cc/vm:vm-reg-set s 1 orig)
    (exec1 (funcall constructor :dst 0 :src 1) s)
    (let ((copy (cl-cc/vm:vm-reg-get s 0)))
      (expect copy :to-equal orig)
      (when deep-p
        (expect (not (eq (first orig) (first copy))) :to-be-truthy))))))
(it-sequential "vm-list-copy-list-cow-write-keeps-original"
  (let ((s (make-test-vm)))
    (let ((orig (list 1 2 3)))
      (cl-cc/vm:vm-reg-set s 1 orig)
      (exec1 (cl-cc/vm:make-vm-copy-list :dst 0 :src 1) s)
      (cl-cc/vm:vm-reg-set s 2 99)
      (exec1 (cl-cc/vm:make-vm-rplaca :cons 0 :val 2) s)
      (expect (= 1 (first orig)) :to-be-truthy)
      (expect (= 99 (car (cl-cc/vm:vm-reg-get s 0))) :to-be-truthy))))
(it-sequential "vm-list-copy-list-cow-nreverse-keeps-original"
  (let ((s (make-test-vm)))
    (let ((orig (list 1 2 3)))
      (cl-cc/vm:vm-reg-set s 1 orig)
      (exec1 (cl-cc/vm:make-vm-copy-list :dst 0 :src 1) s)
      (exec1 (cl-cc/vm:make-vm-nreverse :dst 2 :src 0) s)
      (expect orig :to-equal '(1 2 3))
      (expect (cl-cc/vm:vm-reg-get s 2) :to-equal '(3 2 1)))))
(it-sequential "vm-list-copy-list-cow-nconc-keeps-original"
  (let ((s (make-test-vm)))
    (let ((orig (list 1 2 3)))
      (cl-cc/vm:vm-reg-set s 1 orig)
      (cl-cc/vm:vm-reg-set s 2 (list 4 5))
      (exec1 (cl-cc/vm:make-vm-copy-list :dst 0 :src 1) s)
      (exec1 (cl-cc/vm:make-vm-nconc :dst 3 :lhs 0 :rhs 2) s)
      (expect orig :to-equal '(1 2 3))
      (expect (cl-cc/vm:vm-reg-get s 3) :to-equal '(1 2 3 4 5)))))
(it-sequential "vm-list-subst-replaces"
  (let ((s (make-test-vm)))
    (cl-cc/vm:vm-reg-set s 1 'y)    ; new
    (cl-cc/vm:vm-reg-set s 2 'x)    ; old
    (cl-cc/vm:vm-reg-set s 3 '(a x (b x c)))  ; tree
    (exec1 (cl-cc/vm:make-vm-subst :dst 0 :new-val 1 :old-val 2 :tree 3) s)
    (expect (cl-cc/vm:vm-reg-get s 0) :to-equal '(a y (b y c)))))
(it-sequential "vm-list-type-predicates listp/cons"
  (destructuring-bind (constructor value expected) (list #'cl-cc/vm:make-vm-listp '(a) 1)
    (let ((s (make-test-vm)))
    (%prepare-vm-list-predicate-input s value)
    (exec1 (funcall constructor :dst 0 :src 1) s)
    (expect (= expected (cl-cc/vm:vm-reg-get s 0)) :to-be-truthy))))
(it-sequential "vm-list-type-predicates listp/nil"
  (destructuring-bind (constructor value expected) (list #'cl-cc/vm:make-vm-listp nil 1)
    (let ((s (make-test-vm)))
    (%prepare-vm-list-predicate-input s value)
    (exec1 (funcall constructor :dst 0 :src 1) s)
    (expect (= expected (cl-cc/vm:vm-reg-get s 0)) :to-be-truthy))))
(it-sequential "vm-list-type-predicates listp/atom"
  (destructuring-bind (constructor value expected) (list #'cl-cc/vm:make-vm-listp 42 0)
    (let ((s (make-test-vm)))
    (%prepare-vm-list-predicate-input s value)
    (exec1 (funcall constructor :dst 0 :src 1) s)
    (expect (= expected (cl-cc/vm:vm-reg-get s 0)) :to-be-truthy))))
(it-sequential "vm-list-type-predicates listp/cow-list"
  (destructuring-bind (constructor value expected) (list #'cl-cc/vm:make-vm-listp :cow-list 1)
    (let ((s (make-test-vm)))
    (%prepare-vm-list-predicate-input s value)
    (exec1 (funcall constructor :dst 0 :src 1) s)
    (expect (= expected (cl-cc/vm:vm-reg-get s 0)) :to-be-truthy))))
(it-sequential "vm-list-type-predicates atom/number"
  (destructuring-bind (constructor value expected) (list #'cl-cc/vm:make-vm-atom 42 1)
    (let ((s (make-test-vm)))
    (%prepare-vm-list-predicate-input s value)
    (exec1 (funcall constructor :dst 0 :src 1) s)
    (expect (= expected (cl-cc/vm:vm-reg-get s 0)) :to-be-truthy))))
(it-sequential "vm-list-type-predicates atom/cons"
  (destructuring-bind (constructor value expected) (list #'cl-cc/vm:make-vm-atom '(a) 0)
    (let ((s (make-test-vm)))
    (%prepare-vm-list-predicate-input s value)
    (exec1 (funcall constructor :dst 0 :src 1) s)
    (expect (= expected (cl-cc/vm:vm-reg-get s 0)) :to-be-truthy))))
(it-sequential "vm-list-type-predicates atom/cow-list"
  (destructuring-bind (constructor value expected) (list #'cl-cc/vm:make-vm-atom :cow-list 0)
    (let ((s (make-test-vm)))
    (%prepare-vm-list-predicate-input s value)
    (exec1 (funcall constructor :dst 0 :src 1) s)
    (expect (= expected (cl-cc/vm:vm-reg-get s 0)) :to-be-truthy))))
(it-sequential "vm-list-instructions-advance-pc"
  (let ((s (make-test-vm)))
    (cl-cc/vm:vm-reg-set s 1 '(a b c))
    (cl-cc/vm:vm-reg-set s 2 0)
    (multiple-value-bind (new-pc) (exec1 (cl-cc/vm:make-vm-length :dst 0 :src 1) s 5)
      (expect (= 6 new-pc) :to-be-truthy))
    (multiple-value-bind (new-pc) (exec1 (cl-cc/vm:make-vm-reverse :dst 0 :src 1) s 10)
      (expect (= 11 new-pc) :to-be-truthy))))
)

;;; ─── equal / nconc / copy-list / copy-tree / subst ──────────────────────────

;;; ─── Type predicates ────────────────────────────────────────────────────────

;;; ─── PC advancement ─────────────────────────────────────────────────────────

