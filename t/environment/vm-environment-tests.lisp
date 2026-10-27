;;;; tests/unit/vm/vm-environment-tests.lisp
;;;; Unit tests for src/vm/vm-environment.lisp
;;;;
;;;; Covers: vm-boundp, vm-fboundp, vm-makunbound,
;;;;   vm-fdefinition, vm-random, vm-make-random-state,
;;;;   vm-get-universal-time, vm-get-internal-real-time,
;;;;   vm-get-internal-run-time, vm-decode-universal-time,
;;;;   vm-encode-universal-time.

(in-package #:cl-cc/vm/environment/test)
(defvar *str-package-lock*
  (sb-thread:make-mutex :name "cl-cc symbol test package lock"))
;;; ─── FR-895: Symbol Table Compaction ─────────────────────────────────────────

;;; ─── FR-896: Package Lock / Sealed ──────────────────────────────────────────

(defun %str-delete-package-if-exists (designator)
  "Delete DESIGNATOR's package when it exists, unlocking first if necessary."
  (let ((pkg (find-package designator)))
    (when pkg
      (cl-cc/vm/environment::unlock-package pkg)
      (delete-package pkg))))

;;; ─── helpers ─────────────────────────────────────────────────────────────

(defun %env-unary (ctor-fn sym)
  "Run a unary environment instruction using SYM as the :src register value."
  (let ((s (make-test-vm)))
    (cl-cc/vm:vm-reg-set s 1 sym)
    (exec1 (funcall ctor-fn :dst 0 :src 1) s)
    (cl-cc/vm:vm-reg-get s 0)))

(defun register-vm-environment-tests ()
  (it-sequential "vm-boundp-unbound-symbol-returns-nil"
  (let ((result (%env-unary #'cl-cc/vm:make-vm-boundp 'totally-unbound-sym-xyz)))
    (expect result :to-be-falsy))) (it-sequential "vm-boundp-bound-symbol-returns-t"
  (let ((s (make-test-vm)))
    (setf (gethash 'my-test-var (cl-cc/vm/execution:vm-global-vars s)) 42)
    (cl-cc/vm:vm-reg-set s 1 'my-test-var)
    (exec1 (cl-cc/vm:make-vm-boundp :dst 0 :src 1) s)
    (expect (cl-cc/vm:vm-reg-get s 0) :to-be-truthy))) (it-sequential "vm-boundp-nil-value-still-bound"
  (let ((s (make-test-vm)))
    (setf (gethash 'nil-valued-var (cl-cc/vm/execution:vm-global-vars s)) nil)
    (cl-cc/vm:vm-reg-set s 1 'nil-valued-var)
    (exec1 (cl-cc/vm:make-vm-boundp :dst 0 :src 1) s)
    (expect (cl-cc/vm:vm-reg-get s 0) :to-be-truthy))) (it-sequential "vm-fboundp-unregistered-symbol-returns-nil"
  (let ((result (%env-unary #'cl-cc/vm:make-vm-fboundp 'no-such-function-xyz)))
    (expect result :to-be-falsy))) (it-sequential "vm-fboundp-registered-function-returns-t"
  (let ((s (make-test-vm)))
    (setf (gethash 'my-fn (cl-cc/vm/execution:vm-function-registry s)) #'identity)
    (cl-cc/vm:vm-reg-set s 1 'my-fn)
    (exec1 (cl-cc/vm:make-vm-fboundp :dst 0 :src 1) s)
    (expect (cl-cc/vm:vm-reg-get s 0) :to-be-truthy))) (it-sequential "vm-makunbound-removes-binding-and-returns-sym"
  (let ((s (make-test-vm)))
    (setf (gethash 'to-unbind (cl-cc/vm/execution:vm-global-vars s)) 99)
    (cl-cc/vm:vm-reg-set s 1 'to-unbind)
    (exec1 (cl-cc/vm:make-vm-makunbound :dst 0 :src 1) s)
    (expect (cl-cc/vm:vm-reg-get s 0) :to-be 'to-unbind)
    (expect (nth-value 1 (gethash 'to-unbind (cl-cc/vm/execution:vm-global-vars s))) :to-be-falsy))) (it-sequential "vm-makunbound-already-unbound-returns-sym"
  (let ((result (%env-unary #'cl-cc/vm:make-vm-makunbound 'never-was-bound)))
    (expect result :to-be 'never-was-bound))) (it-sequential "vm-fdefinition-retrieves-registered-function"
  (let ((s (make-test-vm))
        (fn #'identity))
    (setf (gethash 'my-ident (cl-cc/vm/execution:vm-function-registry s)) fn)
    (cl-cc/vm:vm-reg-set s 1 'my-ident)
    (exec1 (cl-cc/vm:make-vm-fdefinition :dst 0 :src 1) s)
    (expect (cl-cc/vm:vm-reg-get s 0) :to-be fn))) (it-sequential "vm-fdefinition-undefined-signals-error"
  (signals error (%env-unary #'cl-cc/vm:make-vm-fdefinition 'undefined-fn-xyz))) (it-sequential "vm-random-returns-integer-in-range"
  (let ((result (%env-unary #'cl-cc/vm:make-vm-random 100)))
    (expect (integerp result) :to-be-truthy)
    (expect (>= result 0) :to-be-truthy)
    (expect (< result 100) :to-be-truthy))) (it-sequential "vm-random-float-limit"
  (let ((result (%env-unary #'cl-cc/vm:make-vm-random 1.0)))
    (expect (floatp result) :to-be-truthy)
    (expect (>= result 0.0) :to-be-truthy)
    (expect (< result 1.0) :to-be-truthy))) (it-sequential "vm-make-random-state-cases fresh"
  (destructuring-bind (arg) (list t)
    (let ((result (%env-unary #'cl-cc/vm:make-vm-make-random-state arg)))
    (expect (cl-cc/vm:vm-random-state-p result) :to-be-truthy)))) (it-sequential "vm-make-random-state-cases copy"
  (destructuring-bind (arg) (list nil)
    (let ((result (%env-unary #'cl-cc/vm:make-vm-make-random-state arg)))
    (expect (cl-cc/vm:vm-random-state-p result) :to-be-truthy)))) (it-sequential "vm-get-universal-time-returns-positive-integer"
  (let ((s (make-test-vm)))
    (exec1 (cl-cc/vm:make-vm-get-universal-time :dst 0) s)
    (let ((result (cl-cc/vm:vm-reg-get s 0)))
      (expect (integerp result) :to-be-truthy)
      (expect (> result 0) :to-be-truthy)))) (it-sequential "vm-get-internal-real-time-returns-non-negative-integer"
  (let ((s (make-test-vm)))
    (exec1 (cl-cc/vm:make-vm-get-internal-real-time :dst 0) s)
    (let ((result (cl-cc/vm:vm-reg-get s 0)))
      (expect (integerp result) :to-be-truthy)
      (expect (>= result 0) :to-be-truthy)))) (it-sequential "vm-get-internal-run-time-returns-non-negative-integer"
  (let ((s (make-test-vm)))
    (exec1 (cl-cc/vm:make-vm-get-internal-run-time :dst 0) s)
    (let ((result (cl-cc/vm:vm-reg-get s 0)))
      (expect (integerp result) :to-be-truthy)
      (expect (>= result 0) :to-be-truthy)))) (it-sequential "vm-decode-universal-time-stores-9-values"
  (let ((s (make-test-vm))
        (epoch (encode-universal-time 0 0 0 1 1 2000)))
    (cl-cc/vm:vm-reg-set s 1 epoch)
    (exec1 (cl-cc/vm:make-vm-decode-universal-time :dst 0 :src 1) s)
    (expect (= 9 (cl-cc/vm/data:length (cl-cc/vm:vm-values-list s))) :to-be-truthy))) (it-sequential "vm-decode-universal-time-primary-value-is-seconds"
  (let ((s (make-test-vm))
        (epoch (encode-universal-time 30 15 12 1 1 2000)))
    (cl-cc/vm:vm-reg-set s 1 epoch)
    (exec1 (cl-cc/vm:make-vm-decode-universal-time :dst 0 :src 1) s)
    (expect (= 30 (cl-cc/vm:vm-reg-get s 0)) :to-be-truthy))) (it-sequential "vm-encode-universal-time-round-trips-decode"
  (let* ((original (encode-universal-time 5 30 10 15 6 2023 0))
         (s (make-test-vm))
         (args (list 5 30 10 15 6 2023 0)))
    (cl-cc/vm:vm-reg-set s 1 args)
    (exec1 (cl-cc/vm:make-vm-encode-universal-time :dst 0 :args-reg 1) s)
    (expect (= original (cl-cc/vm:vm-reg-get s 0)) :to-be-truthy))) (it-sequential "vm-encode-universal-time-without-timezone"
  (let* ((s (make-test-vm))
         (args (list 0 0 12 1 1 2000)))
    (cl-cc/vm:vm-reg-set s 1 args)
    (exec1 (cl-cc/vm:make-vm-encode-universal-time :dst 0 :args-reg 1) s)
    (expect (integerp (cl-cc/vm:vm-reg-get s 0)) :to-be-truthy))) (it-sequential "sym-lock-package"
  (sb-thread:with-mutex (*str-package-lock*)
    (%str-delete-package-if-exists :fr896-lock-test-a)
    (unwind-protect
         (let ((pkg (make-package :fr896-lock-test-a :use nil)))
           (expect (sb-ext:package-locked-p pkg) :to-be-null)
           (cl-cc/vm/environment::lock-package pkg)
           (expect (sb-ext:package-locked-p pkg) :to-be-truthy)
           (cl-cc/vm/environment::unlock-package pkg)
           (expect (sb-ext:package-locked-p pkg) :to-be-null))
      (%str-delete-package-if-exists :fr896-lock-test-a)))) (it-sequential "sym-package-locked-error-on-intern"
  (sb-thread:with-mutex (*str-package-lock*)
    (%str-delete-package-if-exists :fr896-lock-test-b)
    (unwind-protect
         (let ((pkg (make-package :fr896-lock-test-b :use nil)))
           (cl-cc/vm/environment::lock-package pkg)
           (signals cl-cc/vm/environment::package-locked-error (intern "LOCKED-SYMBOL" pkg))
           ;; Package should remain locked
           (expect (sb-ext:package-locked-p pkg) :to-be-truthy))
      (%str-delete-package-if-exists :fr896-lock-test-b)))) (it-sequential "sym-check-package-lock-signals"
  (sb-thread:with-mutex (*str-package-lock*)
    (%str-delete-package-if-exists :fr896-lock-test-d)
    (unwind-protect
         (let ((pkg (make-package :fr896-lock-test-d :use nil)))
           (cl-cc/vm/environment::lock-package pkg)
           (signals cl-cc/vm/environment::package-locked-error (cl-cc/vm/environment::check-package-lock pkg :intern))
            (cl-cc/vm/environment::unlock-package pkg)
            ;; Unlocked should not signal
            (expect (null (cl-cc/vm/environment::check-package-lock pkg :intern)) :to-be-truthy))
       (%str-delete-package-if-exists :fr896-lock-test-d)))) (it-sequential "vm2-state-creation-register-array-and-globals"
                 (let ((s (cl-cc/vm:make-vm2-state)))
                   (expect (simple-vector-p (cl-cc/vm:vm2-state-registers s)) :to-be-truthy)
                   (expect (= cl-cc/vm/execution:+vm-register-count+ (cl-cc/vm/data:length (cl-cc/vm:vm2-state-registers s))) :to-be-truthy)
                   (expect (svref (cl-cc/vm:vm2-state-registers s) 0) :to-be-null)
                   (expect (svref (cl-cc/vm:vm2-state-registers s) 255) :to-be-null)
                   (expect (hash-table-p (cl-cc/vm:vm2-state-global-vars s)) :to-be-truthy)
                   (expect (nth-value 1 (gethash 'cl-cc/vm/environment:*features* (cl-cc/vm:vm2-state-global-vars s))) :to-be-truthy)
                   (expect (cl-cc/vm:vm2-state-output-stream s) :to-be *standard-output*))) (it-sequential "vm2-state-init-vm-global-vars-contains-features"
                 (let ((s (cl-cc/vm:make-vm2-state)))
                   (expect (hash-table-p (cl-cc/vm/execution:vm-global-vars s)) :to-be-truthy)
                   (expect (not (null (gethash 'cl-cc/vm/environment:*features* (cl-cc/vm/execution:vm-global-vars s)))) :to-be-truthy))) (it-sequential "vm2-state-structure"
                 (let ((s (cl-cc/vm:make-vm2-state)))
                   (expect (cl-cc/vm:vm2-state-p s) :to-be-truthy)
                   (expect (simple-vector-p (cl-cc/vm:vm2-state-registers s)) :to-be-truthy)
                   (expect (= 256 (cl-cc/vm/data:length (cl-cc/vm:vm2-state-registers s))) :to-be-truthy)
                   (dotimes (i 256)
                     (expect (null (cl-cc/vm/protocols:vm-reg-get s i)) :to-be-truthy))
                   (expect (cl-cc/vm:vm2-state-values-buffer s) :to-be-null)
                   (expect (not (null (gethash 'cl-cc/vm/environment:*features* (cl-cc/vm:vm2-state-global-vars s)))) :to-be-truthy))
                 (let* ((str (make-string-output-stream))
                        (s   (cl-cc/vm:make-vm2-state :output-stream str)))
                   (expect (cl-cc/vm:vm2-state-output-stream s) :to-equal str))) (describe-sequential "cl-cc-vm package locks"
    (it "constructs the SBCL package lock condition with complete initargs"
      (let ((package (make-package (symbol-name (gensym "LOCK-TEST-")) :use nil)))
        (unwind-protect
             (progn
               (cl-cc/vm/environment::vm-lock-package package)
               (let ((condition
                       (handler-case
                           (progn
                             (cl-cc/vm/environment::check-package-lock package)
                             nil)
                         (sb-ext:package-locked-error (condition) condition))))
                 (expect condition :to-be-truthy)))
          (cl-cc/vm/environment::vm-unlock-package package)
          (delete-package package))))))

;;; ─── vm-boundp ───────────────────────────────────────────────────────────


;;; ─── vm-fboundp ──────────────────────────────────────────────────────────


;;; ─── vm-makunbound ───────────────────────────────────────────────────────


;;; ─── vm-fdefinition ──────────────────────────────────────────────────────


;;; ─── vm-random ───────────────────────────────────────────────────────────


;;; ─── vm-make-random-state ────────────────────────────────────────────────


;;; ─── vm-get-universal-time ───────────────────────────────────────────────


;;; ─── vm-get-internal-real-time ───────────────────────────────────────────


;;; ─── vm-get-internal-run-time ────────────────────────────────────────────


;;; ─── vm-decode-universal-time ────────────────────────────────────────────


;;; ─── vm-encode-universal-time ────────────────────────────────────────────

