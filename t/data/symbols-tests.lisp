;;;; tests/unit/vm/symbols-tests.lisp — VM Symbol Instruction Tests

(in-package #:cl-cc/vm/data/test)

;;; ─── Symbol Operations ───────────────────────────────────────────────────

(defun str-vm ()
  "Create a minimal vm-state for symbol tests."
  (make-test-vm))

(defun str-exec (inst state)
  "Execute a single instruction against STATE."
  (cl-cc/vm/protocols:execute-instruction inst state 0 (make-hash-table :test #'equal)))

(defun register-symbols-tests ()
(it-sequential "sym-symbol-name"
  (let ((s (str-vm)))
    (cl-cc/vm/protocols:vm-reg-set s :R1 'hello)
    (str-exec (cl-cc/vm:make-vm-symbol-name :dst :R0 :src :R1) s)
    (expect (cl-cc/vm/protocols:vm-reg-get s :R0) :to-equal "HELLO")))
(it-sequential "sym-make-symbol"
  (let ((s (str-vm)))
    (cl-cc/vm/protocols:vm-reg-set s :R1 "FOO")
    (str-exec (cl-cc/vm:make-vm-make-symbol :dst :R0 :src :R1) s)
    (let ((result (cl-cc/vm/protocols:vm-reg-get s :R0)))
      (expect (symbolp result) :to-be-truthy)
      (expect (symbol-name result) :to-equal "FOO"))))
(it-sequential "sym-intern-symbol"
  (let ((s (str-vm)))
    (cl-cc/vm/protocols:vm-reg-set s :R1 "INTERN-TEST-SYM-12345")
    (str-exec (cl-cc/vm:make-vm-intern-symbol :dst :R0 :src :R1 :pkg nil) s)
    (let ((result (cl-cc/vm/protocols:vm-reg-get s :R0)))
      (expect (symbolp result) :to-be-truthy)
      (expect (symbol-name result) :to-equal "INTERN-TEST-SYM-12345"))))
(it-sequential "sym-find-package"
  (let ((s (str-vm)))
    (cl-cc/vm/protocols:vm-reg-set s :R1 :cl-user)
    (str-exec (cl-cc/vm:make-vm-find-package :dst :R0 :src :R1) s)
    (expect (cl-cc/vm/protocols:vm-reg-get s :R0) :to-be-truthy)))
(it-sequential "sym-find-symbol"
  (let ((s (str-vm)))
    (cl-cc/vm/protocols:vm-reg-set s :R1 "CAR")
    (cl-cc/vm/protocols:vm-reg-set s :R2 :cl)
    (str-exec (cl-cc/vm:make-vm-find-symbol :dst :R0 :src :R1 :pkg :R2) s)
    (let ((result (cl-cc/vm/protocols:vm-reg-get s :R0)))
      (expect (symbolp result) :to-be-truthy)
      (expect (symbol-name result) :to-equal "CAR")
      (expect (second (cl-cc/vm/protocols:vm-values-list s)) :to-equal :external))))
(it-sequential "sym-gensym"
  (let ((s (str-vm)))
    (str-exec (cl-cc/vm:make-vm-gensym-inst :dst :R0) s)
    (let ((result (cl-cc/vm/protocols:vm-reg-get s :R0)))
      (expect (symbolp result) :to-be-truthy)
      (expect (symbol-package result) :to-equal nil))))
(it-sequential "sym-keywordp keyword"
  (destructuring-bind (value expected) (list :test 1)
    (let ((s (str-vm)))
    (cl-cc/vm/protocols:vm-reg-set s :R1 value)
    (str-exec (cl-cc/vm:make-vm-keywordp :dst :R0 :src :R1) s)
    (expect (cl-cc/vm/protocols:vm-reg-get s :R0) :to-equal expected))))
(it-sequential "sym-keywordp symbol"
  (destructuring-bind (value expected) (list 'hello 0)
    (let ((s (str-vm)))
    (cl-cc/vm/protocols:vm-reg-set s :R1 value)
    (str-exec (cl-cc/vm:make-vm-keywordp :dst :R0 :src :R1) s)
    (expect (cl-cc/vm/protocols:vm-reg-get s :R0) :to-equal expected))))
(it-sequential "sym-defpackage-local-nicknames-expands"
  (sb-thread:with-mutex (*str-package-lock*)
    (str-delete-package-if-exists :fr275-defpackage-local-user)
    (str-delete-package-if-exists :fr275-defpackage-local-target)
    (unwind-protect
         (progn
           (expect (macroexpand-1
             '(defpackage #:fr275-defpackage-local-user
                (:use #:cl)
                (:local-nicknames (#:ln #:fr275-defpackage-local-target)))) :to-be-truthy)
           (eval '(defpackage #:fr275-defpackage-local-target (:use #:cl)))
           (eval '(defpackage #:fr275-defpackage-local-user
                   (:use #:cl)
                   (:local-nicknames (#:ln #:fr275-defpackage-local-target))))
           (let ((*package* (find-package :fr275-defpackage-local-user)))
             (expect (find-package :ln) :to-be (find-package :fr275-defpackage-local-target))))
      (str-delete-package-if-exists :fr275-defpackage-local-user)
      (str-delete-package-if-exists :fr275-defpackage-local-target))))
(it-sequential "sym-vm-add-package-local-nickname"
  (str-with-local-nickname-packages
   "FR275-VM-ADD-TARGET"
   "FR275-VM-ADD-USER"
   (lambda (target user)
     (let ((s (str-vm))
           (ctor (str-vm-constructor "MAKE-VM-ADD-PACKAGE-LOCAL-NICKNAME")))
       (cl-cc/vm/protocols:vm-reg-set s :R1 "LN")
       (cl-cc/vm/protocols:vm-reg-set s :R2 target)
       (cl-cc/vm/protocols:vm-reg-set s :R3 user)
       (str-exec (funcall ctor :dst :R0 :pkg :R3 :nick :R1 :target :R2) s)
       (let ((*package* user))
         (expect (find-package "LN") :to-be target))))))
(it-sequential "sym-vm-remove-package-local-nickname"
  (str-with-local-nickname-packages
   "FR275-VM-REMOVE-TARGET"
   "FR275-VM-REMOVE-USER"
   (lambda (target user)
     (let ((s (str-vm))
           (add-ctor (str-vm-constructor "MAKE-VM-ADD-PACKAGE-LOCAL-NICKNAME"))
           (remove-ctor (str-vm-constructor "MAKE-VM-REMOVE-PACKAGE-LOCAL-NICKNAME")))
       (cl-cc/vm/protocols:vm-reg-set s :R1 "LN")
       (cl-cc/vm/protocols:vm-reg-set s :R2 target)
       (cl-cc/vm/protocols:vm-reg-set s :R3 user)
       (str-exec (funcall add-ctor :dst :R0 :pkg :R3 :nick :R1 :target :R2) s)
       (str-exec (funcall remove-ctor :dst :R0 :pkg :R3 :nick :R1 :target nil) s)
       (let ((*package* user))
         (expect (find-package "LN") :to-be-null))))))
(it-sequential "sym-vm-find-package-uses-local-nickname"
  (str-with-local-nickname-packages
   "FR275-VM-RESOLVE-TARGET"
   "FR275-VM-RESOLVE-USER"
   (lambda (target user)
     (let ((s (str-vm))
           (add-ctor (str-vm-constructor "MAKE-VM-ADD-PACKAGE-LOCAL-NICKNAME")))
       (cl-cc/vm/protocols:vm-reg-set s :R1 "LN")
       (cl-cc/vm/protocols:vm-reg-set s :R2 target)
       (cl-cc/vm/protocols:vm-reg-set s :R3 user)
       (str-exec (funcall add-ctor :dst :R0 :pkg :R3 :nick :R1 :target :R2) s)
       (setf (gethash '*package* (cl-cc/vm:vm-global-vars s)) user)
       (let ((*package* user))
         (str-exec (cl-cc/vm:make-vm-find-package :dst :R0 :src :R1) s))
        (expect (cl-cc/vm/protocols:vm-reg-get s :R0) :to-be target)))))
(it-sequential "sym-self-host-find-package-works-with-runtime-registry"
  (let ((cl-cc/vm/protocols:*vm-self-host-mode* t)
        (s (str-vm)))
    ;; Register a package in the runtime registry
    (cl-cc/runtime:rt-make-package :self-host-test-pkg)
    (cl-cc/vm/protocols:vm-reg-set s :R1 :self-host-test-pkg)
    (str-exec (cl-cc/vm:make-vm-find-package :dst :R0 :src :R1) s)
    (expect (cl-cc/vm/protocols:vm-reg-get s :R0) :to-be-truthy)))
(it-sequential "sym-default-locked-packages"
  (expect (find-package :cl) :to-be-truthy)
  (expect (sb-ext:package-locked-p (find-package :cl)) :to-be-truthy))
(it-sequential "sym-self-host-intern-works-with-runtime-registry"
  (let ((cl-cc/vm/protocols:*vm-self-host-mode* t)
        (s (str-vm)))
    (cl-cc/vm/protocols:vm-reg-set s :R1 "SELF-HOST-INTERN-SYM")
    (str-exec (cl-cc/vm:make-vm-intern-symbol :dst :R0 :src :R1 :pkg nil) s)
    (let ((result (cl-cc/vm/protocols:vm-reg-get s :R0)))
      (expect (symbolp result) :to-be-truthy)
      (expect (symbol-name result) :to-equal "SELF-HOST-INTERN-SYM"))))
)

;;; ─── Package-Local Nicknames (FR-275) ─────────────────────────────────────

(defun str-delete-package-if-exists (designator)
  "Delete DESIGNATOR's package when it exists."
  (let ((package (find-package designator)))
    (when package
      (delete-package package))))

(defun str-vm-constructor (name)
  "Return exported VM constructor NAME from CL-CC, or skip until FR-275 exists."
  (multiple-value-bind (symbol status)
      (find-symbol name :cl-cc)
    (unless (and symbol (eq status :external) (fboundp symbol))
      (skip (format nil "FR-275 constructor ~A is not available yet" name)))
    (symbol-function symbol)))

(defvar *str-package-lock*
  (sb-thread:make-mutex :name "cl-cc symbol test package lock"))

(defun str-with-local-nickname-packages (target-name user-name thunk)
  "Run THUNK with fresh TARGET-NAME and USER-NAME packages, then clean up."
  (sb-thread:with-mutex (*str-package-lock*)
    (str-delete-package-if-exists user-name)
    (str-delete-package-if-exists target-name)
    (let ((target (make-package target-name :use nil))
          (user (make-package user-name :use nil)))
      (unwind-protect
           (funcall thunk target user)
        (str-delete-package-if-exists user-name)
        (str-delete-package-if-exists target-name)))))

;;; ─── FR-895: Symbol Table Compaction ─────────────────────────────────────────

;;; ─── FR-896: Package Lock / Sealed ──────────────────────────────────────────

;;; ─── Self-Host Mode (FR-626) ──────────────────────────────────────────────

