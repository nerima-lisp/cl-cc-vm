;;;; tests/unit/vm/vm-run-fusion-tests.lisp — VM2 superinstruction fusion and extended run-vm tests

(in-package #:cl-cc/vm/execution/test)


(progn
(defclass vm2-test-instance ()
  ((x :initarg :x :reader vm2-test-instance-x)))
(defun register-vm-run-fusion-tests ()
  (it-sequential "vm2-bigram-collection-counts-pairs"
                 (let* ((code (make-bytecode cl-cc/vm:+op2-const+ 1 3 nil
                                             cl-cc/vm:+op2-add-imm2+ 0 1 4
                                             cl-cc/vm:+op2-halt2+ 0 nil nil
                                             cl-cc/vm:+op2-const+ 1 5 nil
                                             cl-cc/vm:+op2-add-imm2+ 0 1 6
                                             cl-cc/vm:+op2-halt2+ 0 nil nil))
                        (counts (cl-cc/vm::vm2-collect-opcode-bigrams code)))
                   (expect (= 2 (gethash '(cl-cc/vm/execution::const cl-cc/vm/execution:add-imm2) counts)) :to-be-truthy)
                   (expect (= 2 (gethash '(cl-cc/vm/execution:add-imm2 cl-cc/vm:halt2) counts)) :to-be-truthy)))

  (it-sequential "vm2-fuse-immediate-superinstructions-arith add"
                 (destructuring-bind (c0 r0 v0 x0  c1 d1 s1 s2  expected-fused-op expected-imm) (list cl-cc/vm:+op2-const+ 2 4 nil cl-cc/vm:+op2-add2+ 0 1 2 cl-cc/vm:+op2-add-imm2+ 4)
                   (let* ((code  (make-bytecode c0 r0 v0 x0  c1 d1 s1 s2  cl-cc/vm:+op2-halt2+ 0 nil nil))
                          (fused (cl-cc/vm::vm2-fuse-immediate-superinstructions code)))
                     (expect (= expected-fused-op (aref fused 0)) :to-be-truthy)
                     (expect (= expected-imm (aref fused 3)) :to-be-truthy))))

  (it-sequential "vm2-fuse-immediate-superinstructions-arith sub"
                 (destructuring-bind (c0 r0 v0 x0  c1 d1 s1 s2  expected-fused-op expected-imm) (list cl-cc/vm:+op2-const+ 2 5 nil cl-cc/vm:+op2-sub2+ 0 1 2 cl-cc/vm:+op2-sub-imm2+ 5)
                   (let* ((code  (make-bytecode c0 r0 v0 x0  c1 d1 s1 s2  cl-cc/vm:+op2-halt2+ 0 nil nil))
                          (fused (cl-cc/vm::vm2-fuse-immediate-superinstructions code)))
                     (expect (= expected-fused-op (aref fused 0)) :to-be-truthy)
                     (expect (= expected-imm (aref fused 3)) :to-be-truthy))))

  (it-sequential "vm2-fuse-immediate-superinstructions-arith mul"
                 (destructuring-bind (c0 r0 v0 x0  c1 d1 s1 s2  expected-fused-op expected-imm) (list cl-cc/vm:+op2-const+ 2 6 nil cl-cc/vm:+op2-mul2+ 0 2 1 cl-cc/vm:+op2-mul-imm2+ 6)
                   (let* ((code  (make-bytecode c0 r0 v0 x0  c1 d1 s1 s2  cl-cc/vm:+op2-halt2+ 0 nil nil))
                          (fused (cl-cc/vm::vm2-fuse-immediate-superinstructions code)))
                     (expect (= expected-fused-op (aref fused 0)) :to-be-truthy)
                     (expect (= expected-imm (aref fused 3)) :to-be-truthy))))

  (it-sequential "vm2-fuse-immediate-superinstructions-const-halt"
                 (let* ((code (make-bytecode cl-cc/vm:+op2-const+ 0 42 nil
                                             cl-cc/vm:+op2-halt2+ 0 nil nil))
                        (fused (cl-cc/vm::vm2-fuse-immediate-superinstructions code)))
                   (expect (= cl-cc/vm:+op2-const-halt2+ (aref fused 0)) :to-be-truthy)
                   (expect (= 42 (aref fused 1)) :to-be-truthy)
                   (expect (= 4 (length fused)) :to-be-truthy)))

  (it-sequential "vm2-fuse-immediate-superinstructions-compare"
                 (let* ((eq-code (make-bytecode cl-cc/vm:+op2-const+ 2 6 nil
                                                cl-cc/vm:+op2-num-eq2+ 0 1 2
                                                cl-cc/vm:+op2-halt2+ 0 nil nil))
                        (lt-code (make-bytecode cl-cc/vm:+op2-const+ 2 6 nil
                                                cl-cc/vm:+op2-num-lt2+ 0 1 2
                                                cl-cc/vm:+op2-halt2+ 0 nil nil))
                        (gt-code (make-bytecode cl-cc/vm:+op2-const+ 2 6 nil
                                                cl-cc/vm:+op2-num-gt2+ 0 1 2
                                                cl-cc/vm:+op2-halt2+ 0 nil nil))
                        (le-code (make-bytecode cl-cc/vm:+op2-const+ 2 6 nil
                                                cl-cc/vm:+op2-num-le2+ 0 1 2
                                                cl-cc/vm:+op2-halt2+ 0 nil nil))
                        (ge-code (make-bytecode cl-cc/vm:+op2-const+ 2 6 nil
                                                cl-cc/vm:+op2-num-ge2+ 0 1 2
                                                cl-cc/vm:+op2-halt2+ 0 nil nil)))
                   (expect (= cl-cc/vm:+op2-num-eq-imm2+ (aref (cl-cc/vm::vm2-fuse-immediate-superinstructions eq-code) 0)) :to-be-truthy)
                   (expect (= cl-cc/vm:+op2-num-lt-imm2+ (aref (cl-cc/vm::vm2-fuse-immediate-superinstructions lt-code) 0)) :to-be-truthy)
                   (expect (= cl-cc/vm:+op2-num-gt-imm2+ (aref (cl-cc/vm::vm2-fuse-immediate-superinstructions gt-code) 0)) :to-be-truthy)
                   (expect (= cl-cc/vm:+op2-num-le-imm2+ (aref (cl-cc/vm::vm2-fuse-immediate-superinstructions le-code) 0)) :to-be-truthy)
                   (expect (= cl-cc/vm:+op2-num-ge-imm2+ (aref (cl-cc/vm::vm2-fuse-immediate-superinstructions ge-code) 0)) :to-be-truthy)))

  (it-sequential "vm2-state-init-pre-populates-globals"
                 (let ((s (cl-cc/vm:make-vm2-state)))
                   (multiple-value-bind (val found-p)
                       (gethash 'cl-cc/vm:*active-restarts* (cl-cc/vm:vm2-state-global-vars s))
                     (expect found-p :to-be-truthy)
                     (expect val :to-be-null))
                   (expect (not (null (gethash '*standard-output* (cl-cc/vm:vm2-state-global-vars s)))) :to-be-truthy)))

  (it-sequential "vm2-state-init-accepts-custom-output-stream"
                 (let* ((str (make-string-output-stream))
                        (s   (cl-cc/vm:make-vm2-state :output-stream str)))
                   (expect (cl-cc/vm:vm2-state-output-stream s) :to-be str)))

  (it-sequential "vm2-state-init-vm-global-vars-contains-features"
                 (let ((s (cl-cc/vm:make-vm2-state)))
                   (expect (hash-table-p (cl-cc/vm::vm-global-vars s)) :to-be-truthy)
                   (expect (not (null (gethash 'cl-cc/vm::*features* (cl-cc/vm::vm-global-vars s)))) :to-be-truthy)))



  ))
