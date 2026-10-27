;;;; tests/unit/vm/conditions-tests.lisp — VM Condition System Unit Tests
;;;;
;;;; Tests for handler stack management, restart bindings,
;;;; condition construction, and signal dispatch.

(in-package #:cl-cc/vm/conditions/test)

(defun register-conditions-tests ()
  (it-sequential "vm-condition-constructor-slots type-error"
  (destructuring-bind (make-fn expected-type check-slots-fn) (list (lambda (s) (cl-cc/vm/conditions:make-vm-type-error s 'fixnum "hello")) 'cl-cc/vm:vm-type-error (lambda (c) (and (equal 'fixnum (type-error-expected-type c))
                            (equal "hello" (type-error-datum c)))))
    (let* ((state (make-instance 'cl-cc/vm/io:vm-io-state))
         (cond (funcall make-fn state)))
    (expect (typep cond expected-type) :to-be-truthy)
    (expect (funcall check-slots-fn cond) :to-be-truthy)))) (it-sequential "vm-condition-constructor-slots unbound-variable"
  (destructuring-bind (make-fn expected-type check-slots-fn) (list (lambda (s) (cl-cc/vm/conditions:make-vm-unbound-variable s 'x)) 'cl-cc/vm:vm-unbound-variable (lambda (c) (equal 'x (cell-error-name c))))
    (let* ((state (make-instance 'cl-cc/vm/io:vm-io-state))
         (cond (funcall make-fn state)))
    (expect (typep cond expected-type) :to-be-truthy)
    (expect (funcall check-slots-fn cond) :to-be-truthy)))) (it-sequential "vm-condition-constructor-slots undefined-function"
  (destructuring-bind (make-fn expected-type check-slots-fn) (list (lambda (s) (cl-cc/vm/conditions:make-vm-undefined-function s 'foo)) 'cl-cc/vm:vm-undefined-function (lambda (c) (equal 'foo (cell-error-name c))))
    (let* ((state (make-instance 'cl-cc/vm/io:vm-io-state))
         (cond (funcall make-fn state)))
    (expect (typep cond expected-type) :to-be-truthy)
    (expect (funcall check-slots-fn cond) :to-be-truthy)))) (it-sequential "vm-condition-constructor-slots division-by-zero"
  (destructuring-bind (make-fn expected-type check-slots-fn) (list (lambda (s) (cl-cc/vm/conditions:make-vm-division-by-zero s 42)) 'cl-cc/vm:vm-division-by-zero (lambda (c) (equal 42 (cl-cc/vm/conditions:vm-dividend c))))
    (let* ((state (make-instance 'cl-cc/vm/io:vm-io-state))
         (cond (funcall make-fn state)))
    (expect (typep cond expected-type) :to-be-truthy)
    (expect (funcall check-slots-fn cond) :to-be-truthy)))) (it-sequential "condition-inherits-vm-error type-error"
  (destructuring-bind (make-cond-fn) (list (lambda (s) (cl-cc/vm/conditions:make-vm-type-error s 'fixnum 42)))
    (let* ((state (make-instance 'cl-cc/vm/io:vm-io-state))
         (c (funcall make-cond-fn state)))
    (expect (typep c 'cl-cc/vm:vm-error) :to-be-truthy)))) (it-sequential "condition-inherits-vm-error unbound-var"
  (destructuring-bind (make-cond-fn) (list (lambda (s) (cl-cc/vm/conditions:make-vm-unbound-variable s 'x)))
    (let* ((state (make-instance 'cl-cc/vm/io:vm-io-state))
         (c (funcall make-cond-fn state)))
    (expect (typep c 'cl-cc/vm:vm-error) :to-be-truthy)))) (it-sequential "condition-inherits-vm-error division-by-zero"
  (destructuring-bind (make-cond-fn) (list (lambda (s) (cl-cc/vm/conditions:make-vm-division-by-zero s 42)))
    (let* ((state (make-instance 'cl-cc/vm/io:vm-io-state))
         (c (funcall make-cond-fn state)))
    (expect (typep c 'cl-cc/vm:vm-error) :to-be-truthy)))) (it-sequential "type-error-is-printable"
  (let* ((state (make-instance 'cl-cc/vm/io:vm-io-state))
         (cond (cl-cc/vm/conditions:make-vm-type-error state 'fixnum "hello"))
         (msg (format nil "~A" cond)))
    (expect (stringp msg) :to-be-truthy))) (it-sequential "handler-stack-fresh-state-is-empty"
  (let ((state (make-instance 'cl-cc/vm/io:vm-io-state)))
    (expect (cl-cc/vm/conditions:vm-get-handler-stack state) :to-equal nil))) (it-sequential "handler-stack-push-pop-roundtrip"
  (let ((state (make-instance 'cl-cc/vm/io:vm-io-state)))
    (cl-cc/vm/conditions:vm-push-handler-to-stack state 'cl-cc/vm:vm-error #'identity)
    (let ((handler (cl-cc/vm/conditions:vm-pop-handler-from-stack state)))
      (expect (not (null handler)) :to-be-truthy)
      (expect (cl-cc/vm/conditions:vm-handler-type handler) :to-equal 'cl-cc/vm:vm-error)))) (it-sequential "handler-stack-pop-on-empty-returns-nil"
  (let ((state (make-instance 'cl-cc/vm/io:vm-io-state)))
    (expect (cl-cc/vm/conditions:vm-pop-handler-from-stack state) :to-equal nil))) (it-sequential "handler-stack-lifo-order-preserved"
  (let ((state (make-instance 'cl-cc/vm/io:vm-io-state)))
    (cl-cc/vm/conditions:vm-push-handler-to-stack state 'cl-cc/vm:vm-error #'identity)
    (cl-cc/vm/conditions:vm-push-handler-to-stack state 'cl-cc/vm:vm-warning #'identity)
    (let ((first (cl-cc/vm/conditions:vm-pop-handler-from-stack state)))
      (expect (cl-cc/vm/conditions:vm-handler-type first) :to-equal 'cl-cc/vm:vm-warning))
    (let ((second (cl-cc/vm/conditions:vm-pop-handler-from-stack state)))
      (expect (cl-cc/vm/conditions:vm-handler-type second) :to-equal 'cl-cc/vm:vm-error)))) (it-sequential "find-handler-behavior"
  (let ((state (make-instance 'cl-cc/vm/io:vm-io-state))
        (cond-val nil))
    (cl-cc/vm/conditions:vm-push-handler-to-stack state 'cl-cc/vm:vm-error #'identity)
    (setf cond-val (cl-cc/vm/conditions:make-vm-type-error state 'fixnum 42))
    (expect (not (null (cl-cc/vm/conditions:vm-find-handler state cond-val))) :to-be-truthy)
    ;; replace with non-matching handler
    (cl-cc/vm/conditions:vm-pop-handler-from-stack state)
    (cl-cc/vm/conditions:vm-push-handler-to-stack state 'cl-cc/vm:vm-warning #'identity)
    (expect (cl-cc/vm/conditions:vm-find-handler state cond-val) :to-equal nil))) (it-sequential "vm-restart-operations"
  (let ((state (make-instance 'cl-cc/vm/io:vm-io-state)))
    (expect (cl-cc/vm/conditions:vm-get-restarts state) :to-equal nil))
  (let ((state (make-instance 'cl-cc/vm/io:vm-io-state)))
    (cl-cc/vm/conditions:vm-add-restart state 'continue #'identity)
    (let ((restart (cl-cc/vm/conditions:vm-find-restart state 'continue)))
      (expect (not (null restart)) :to-be-truthy)
      (expect (cl-cc/vm/conditions:vm-restart-name restart) :to-equal 'continue)))
  (let ((state (make-instance 'cl-cc/vm/io:vm-io-state)))
    (expect (cl-cc/vm/conditions:vm-find-restart state 'nonexistent) :to-equal nil))) (it-sequential "signal-condition-dispatch"
  (let* ((state (make-instance 'cl-cc/vm/io:vm-io-state))
         (cond-val (cl-cc/vm/conditions:make-vm-type-error state 'fixnum 42)))
    (cl-cc/vm/conditions:vm-push-handler-to-stack state 'cl-cc/vm:vm-error #'identity)
    (multiple-value-bind (found handler)
        (cl-cc/vm/conditions:vm-signal-condition cond-val state)
      (expect found :to-be-truthy)
      (expect (not (null handler)) :to-be-truthy))
    ;; remove handler, signal again — no match
    (cl-cc/vm/conditions:vm-pop-handler-from-stack state)
    (multiple-value-bind (found handler)
        (cl-cc/vm/conditions:vm-signal-condition cond-val state)
      (expect found :to-equal nil)
      (expect handler :to-equal nil)))) (it-sequential "signal-condition-error-p-signals"
  (let ((state (make-instance 'cl-cc/vm/io:vm-io-state)))
    (let ((cond (cl-cc/vm/conditions:make-vm-type-error state 'fixnum 42)))
      (expect (handler-case
           (progn (cl-cc/vm/conditions:vm-signal-condition cond state :error-p t) nil)
         (cl-cc/vm:vm-type-error () t)) :to-be-truthy)))) (it-sequential "clear-condition-context"
  (let ((state (make-instance 'cl-cc/vm/io:vm-io-state)))
    (cl-cc/vm/conditions:vm-push-handler-to-stack state 'cl-cc/vm:vm-error #'identity)
    (cl-cc/vm/conditions:vm-add-restart state 'continue #'identity)
    (cl-cc/vm/conditions:vm-clear-condition-context state)
    (expect (cl-cc/vm/conditions:vm-get-handler-stack state) :to-equal nil)
    (expect (cl-cc/vm/conditions:vm-get-restarts state) :to-equal nil))) (it-sequential "vm-sync-handler-regs-shares-snapshot-across-handler-entries"
  (let ((state (make-instance 'cl-cc/vm/io:vm-io-state)))
    (cl-cc/vm/protocols:vm-reg-set state :r1 10)
    (cl-cc/vm/protocols:execute-instruction
     (cl-cc/vm:make-vm-establish-handler :handler-label "h1" :result-reg :r0 :error-type 'error)
     state 0 nil)
    (cl-cc/vm/protocols:execute-instruction
     (cl-cc/vm:make-vm-establish-handler :handler-label "h2" :result-reg :r0 :error-type 'error)
     state 1 nil)
    (cl-cc/vm/protocols:vm-reg-set state :r1 42)
    (cl-cc/vm/protocols:execute-instruction (cl-cc/vm:make-vm-sync-handler-regs) state 2 nil)
    (let* ((entries (cl-cc/vm/execution:vm-handler-stack state))
           (snapshot-a (cl-cc/vm/execution:vm-handler-entry-saved-regs (first entries)))
           (snapshot-b (cl-cc/vm/execution:vm-handler-entry-saved-regs (second entries))))
      (expect snapshot-b :to-be snapshot-a)
      (expect (= 42 (gethash :r1 snapshot-a)) :to-be-truthy)))) (it-sequential "vm-sync-handler-regs-updates-catch-frame-saved-regs"
  (let ((state (make-instance 'cl-cc/vm/io:vm-io-state)))
    (cl-cc/vm/protocols:vm-reg-set state :tag 7)
    (cl-cc/vm/protocols:vm-reg-set state :r1 99)
    (cl-cc/vm/protocols:execute-instruction
     (cl-cc/vm:make-vm-establish-catch :tag-reg :tag :handler-label "catch" :result-reg :r0)
     state 0 nil)
    (cl-cc/vm/protocols:vm-reg-set state :r1 123)
    (cl-cc/vm/protocols:execute-instruction (cl-cc/vm:make-vm-sync-handler-regs) state 1 nil)
    (let* ((entry (first (cl-cc/vm/execution:vm-handler-stack state)))
           (snapshot (cl-cc/vm/execution:vm-handler-entry-saved-regs entry)))
      (expect (hash-table-p snapshot) :to-be-truthy)
      (expect (= 123 (gethash :r1 snapshot)) :to-be-truthy)))) (it-sequential "vm-establish-handler-retains-call-and-method-stacks"
  (let ((state (make-instance 'cl-cc/vm/io:vm-io-state)))
    (setf (cl-cc/vm/execution:vm-call-stack state) '((1 :r0 nil nil)))
    (setf (cl-cc/vm/execution:vm-method-call-stack state) '((gf nil args)))
    (cl-cc/vm/protocols:execute-instruction
     (cl-cc/vm:make-vm-establish-handler :handler-label "h" :result-reg :r0 :error-type 'error)
     state 0 nil)
    (let ((entry (first (cl-cc/vm/execution:vm-handler-stack state))))
      (expect (cl-cc/vm/execution:vm-call-stack state) :to-be (fourth entry))
      (expect (cl-cc/vm/execution:vm-handler-entry-saved-regs entry) :to-be (fifth entry))
      (expect (cl-cc/vm/execution:vm-method-call-stack state) :to-be (sixth entry))))) (it-sequential "vm-establish-catch-retains-call-and-method-stacks"
  (let ((state (make-instance 'cl-cc/vm/io:vm-io-state)))
    (setf (cl-cc/vm/execution:vm-call-stack state) '((7 :r1 nil nil)))
    (setf (cl-cc/vm/execution:vm-method-call-stack state) '((gf2 nil args2)))
    (cl-cc/vm/protocols:vm-reg-set state :tag 9)
    (cl-cc/vm/protocols:execute-instruction
     (cl-cc/vm:make-vm-establish-catch :tag-reg :tag :handler-label "c" :result-reg :r0)
     state 0 nil)
    (let ((entry (first (cl-cc/vm/execution:vm-handler-stack state))))
      (expect (cl-cc/vm/execution:vm-call-stack state) :to-be (fifth entry))
      (expect (cl-cc/vm/execution:vm-method-call-stack state) :to-be (seventh entry))))) (progn
  (it-sequential "guest make-condition minimal condition"
  (let* ((state (make-test-vm))
         (make-condition-fn (cl-cc/vm/protocols:vm-lookup-bridge-callable 'cl:make-condition)))
    (let ((cl-cc/vm/execution:*vm-current-state* state))
      (let ((condition (funcall make-condition-fn 'condition)))
        (expect (cl-cc/vm:vm-typep-check condition 'condition) :to-be-truthy)))))

  (it-sequential "guest ANSI condition classes constructible"
    (let* ((state (make-test-vm))
           (make-condition-fn (cl-cc/vm/protocols:vm-lookup-bridge-callable 'cl:make-condition)))
      (let ((cl-cc/vm/execution:*vm-current-state* state))
        (dolist (class-name
                 '(condition serious-condition error warning simple-condition
                   simple-error simple-warning simple-type-error type-error
                   arithmetic-error division-by-zero floating-point-inexact
                   floating-point-invalid-operation floating-point-underflow
                   floating-point-overflow cell-error unbound-slot unbound-variable
                   undefined-function control-error file-error package-error
                   parse-error print-not-readable program-error stream-error
                   end-of-file reader-error storage-condition style-warning))
          (expect (cl-cc/vm:vm-typep-check
                   (funcall make-condition-fn class-name)
                   class-name)
                  :to-be-truthy)))))

  (it-sequential "guest ANSI condition hierarchy and class queries"
    (let* ((state (make-test-vm))
           (make-condition-fn (cl-cc/vm/protocols:vm-lookup-bridge-callable 'cl:make-condition))
           (subtypep-fn (cl-cc/vm/protocols:vm-lookup-bridge-callable 'cl:subtypep)))
      (let* ((cl-cc/vm/execution:*vm-current-state* state)
             (condition (funcall make-condition-fn 'simple-error
                                 :format-control "guest error"))
             (registry (cl-cc/vm/execution:vm-class-registry state)))
        (cl-cc/vm:vm-reg-set state :condition condition)
        (cl-cc/vm/protocols:execute-instruction
         (cl-cc/vm:make-vm-class-of-fn :dst :class :src :condition)
         state 0 nil)
        (expect (cl-cc/vm:vm-reg-get state :class)
                :to-be (gethash 'simple-error registry))
        (expect (gethash :__cpl__ (gethash 'simple-error registry))
                :to-equal '(simple-error simple-condition error serious-condition condition t))
        (expect (cl-cc/vm:vm-typep-check condition 'condition) :to-be-truthy)
        (expect (cl-cc/vm:vm-typep-check condition 'serious-condition) :to-be-truthy)
        (expect (cl-cc/vm:vm-typep-check condition 'error) :to-be-truthy)
        (expect (cl-cc/vm:vm-typep-check condition 'simple-condition) :to-be-truthy)
        (expect (cl-cc/vm:vm-typep-check condition 'warning) :to-be-falsy)
        (multiple-value-bind (yes certain)
            (funcall subtypep-fn 'division-by-zero 'arithmetic-error)
          (expect yes :to-be-truthy)
          (expect certain :to-be-truthy))
        (multiple-value-bind (yes certain)
            (funcall subtypep-fn 'warning 'error)
          (expect yes :to-be-falsy)
          (expect certain :to-be-truthy)))))

  (it-sequential "guest ANSI condition slot initargs and readers"
    (let* ((state (make-test-vm))
           (make-condition-fn (cl-cc/vm/protocols:vm-lookup-bridge-callable 'cl:make-condition)))
      (let ((cl-cc/vm/execution:*vm-current-state* state))
        (flet ((make-guest-condition (class &rest initargs)
                 (apply make-condition-fn class initargs))
               (read-condition-slot (reader condition)
                 (funcall (cl-cc/vm/protocols:vm-lookup-bridge-callable reader) condition)))
          (let ((condition (make-guest-condition 'arithmetic-error
                                           :operation '/ :operands '(1 0))))
            (expect (read-condition-slot 'cl:arithmetic-error-operation condition)
                    :to-be '/)
            (expect (read-condition-slot 'cl:arithmetic-error-operands condition)
                    :to-equal '(1 0)))
          (let ((condition (make-guest-condition 'cell-error :name 'missing-cell)))
            (expect (read-condition-slot 'cl:cell-error-name condition)
                    :to-be 'missing-cell))
          (let ((condition (make-guest-condition 'unbound-slot
                                           :name 'field
                                           :instance :guest-instance)))
            (expect (read-condition-slot 'cl:cell-error-name condition) :to-be 'field)
            (expect (read-condition-slot 'cl:unbound-slot-instance condition)
                    :to-be :guest-instance))
          (let ((pathname #p"/tmp/guest-condition")
                (condition nil))
            (setf condition (make-guest-condition 'file-error :pathname pathname))
            (expect (read-condition-slot 'cl:file-error-pathname condition)
                    :to-equal pathname))
          (let ((condition (make-guest-condition 'package-error :package :cl-user)))
            (expect (read-condition-slot 'cl:package-error-package condition)
                    :to-be :cl-user))
          (let* ((object '(guest unreadable object))
                 (condition (make-guest-condition 'print-not-readable :object object)))
            (expect (read-condition-slot 'cl:print-not-readable-object condition)
                    :to-be object))
          (let ((condition (make-guest-condition 'stream-error :stream *standard-output*)))
            (expect (read-condition-slot 'cl:stream-error-stream condition)
                    :to-be *standard-output*))
          (let ((condition (make-guest-condition 'simple-condition
                                           :format-control "guest ~A"
                                           :format-arguments '(condition))))
            (expect (read-condition-slot 'cl:simple-condition-format-control condition)
                    :to-equal "guest ~A")
            (expect (read-condition-slot 'cl:simple-condition-format-arguments condition)
                    :to-equal '(condition)))
          (let ((condition (make-guest-condition 'simple-condition
                                           :format-control "no arguments")))
            (expect (read-condition-slot 'cl:simple-condition-format-arguments condition)
                    :to-equal nil))
          (let ((condition (make-guest-condition 'type-error
                                           :datum :datum
                                           :expected-type 'integer)))
            (expect (read-condition-slot 'cl:type-error-datum condition)
                    :to-be :datum)
            (expect (read-condition-slot 'cl:type-error-expected-type condition)
                    :to-be 'integer))))))

  (progn
  (progn
  (it-sequential "guest ANSI condition direct superclasses"
    (let* ((state (make-test-vm))
           (registry (cl-cc/vm/execution:vm-class-registry state)))
      (dolist (class-spec
               '((condition (t))
                 (serious-condition (condition))
                 (error (serious-condition))
                 (warning (condition))
                 (simple-condition (condition))
                 (simple-error (simple-condition error))
                 (simple-type-error (simple-condition type-error))
                 (simple-warning (simple-condition warning))
                 (type-error (error))
                 (arithmetic-error (error))
                 (division-by-zero (arithmetic-error))
                 (floating-point-inexact (arithmetic-error))
                 (floating-point-invalid-operation (arithmetic-error))
                 (floating-point-underflow (arithmetic-error))
                 (floating-point-overflow (arithmetic-error))
                 (cell-error (error))
                 (unbound-slot (cell-error))
                 (unbound-variable (cell-error))
                 (undefined-function (cell-error))
                 (control-error (error))
                 (file-error (error))
                 (package-error (error))
                 (parse-error (error))
                 (print-not-readable (error))
                 (program-error (error))
                 (stream-error (error))
                 (end-of-file (stream-error))
                 (reader-error (parse-error stream-error))
                 (storage-condition (serious-condition))
                 (style-warning (warning))))
        (expect (gethash :__superclasses__
                         (gethash (first class-spec) registry))
                :to-equal (second class-spec)))))

  (progn
  (it-sequential "guest CLOS condition handler stack matching"
    (let* ((state (make-test-vm))
           (warning-state (make-test-vm))
           (make-condition-fn (cl-cc/vm/protocols:vm-lookup-bridge-callable 'cl:make-condition))
           (handler-fn #'identity))
      (let* ((cl-cc/vm/execution:*vm-current-state* state)
             (condition (funcall make-condition-fn 'simple-error
                                 :format-control "guest error"))
             (handler
               (first
                (cl-cc/vm/conditions:vm-push-handler-to-stack
                 state 'simple-condition handler-fn))))
        (expect (vectorp condition) :to-be-truthy)
        (expect (eq (cl-cc/vm/conditions:vm-find-handler state condition)
                    handler)
                :to-be-truthy))
      (let* ((cl-cc/vm/execution:*vm-current-state* warning-state)
             (condition (funcall make-condition-fn 'warning)))
        (cl-cc/vm/conditions:vm-push-handler-to-stack
         warning-state 'error handler-fn)
        (expect (vectorp condition) :to-be-truthy)
        (expect (null (cl-cc/vm/conditions:vm-find-handler
                       warning-state condition))
                :to-be-truthy))))
  (it-sequential "guest signal dispatches guest warning"
    (let* ((state (make-test-vm))
           (make-condition-fn (cl-cc/vm/protocols:vm-lookup-bridge-callable 'cl:make-condition))
           (handler-fn #'identity)
           (cl-cc/vm/execution:*vm-current-state* state)
           (condition (funcall make-condition-fn 'warning))
           (handler
             (first
              (cl-cc/vm/conditions:vm-push-handler-to-stack
               state 'warning handler-fn))))
      (multiple-value-bind (found matched-handler)
          (cl-cc/vm/conditions:vm-signal-condition condition state)
        (expect (cl-cc/vm:vm-typep-check condition 'warning) :to-be-truthy)
        (expect found :to-be-truthy)
        (expect (eq matched-handler handler) :to-be-truthy))))
  (it-sequential "guest error handler matches guest simple-error"
    (let* ((state (make-test-vm))
           (make-condition-fn (cl-cc/vm/protocols:vm-lookup-bridge-callable 'cl:make-condition))
           (handler-fn #'identity)
           (cl-cc/vm/execution:*vm-current-state* state)
           (condition (funcall make-condition-fn 'simple-error
                               :format-control "guest error"))
           (handler
             (first
              (cl-cc/vm/conditions:vm-push-handler-to-stack
               state 'error handler-fn))))
      (multiple-value-bind (found matched-handler)
          (cl-cc/vm/conditions:vm-signal-condition condition state :error-p t)
        (expect (cl-cc/vm:vm-typep-check condition 'error) :to-be-truthy)
        (expect found :to-be-truthy)
        (expect (eq matched-handler handler) :to-be-truthy))))
  (it-sequential "guest warning does not match guest error handler"
    (let* ((state (make-test-vm))
           (make-condition-fn (cl-cc/vm/protocols:vm-lookup-bridge-callable 'cl:make-condition))
           (cl-cc/vm/execution:*vm-current-state* state)
           (condition (funcall make-condition-fn 'warning)))
      (cl-cc/vm/conditions:vm-push-handler-to-stack state 'error #'identity)
      (multiple-value-bind (found handler)
          (cl-cc/vm/conditions:vm-signal-condition condition state)
        (expect (cl-cc/vm:vm-typep-check condition 'warning) :to-be-truthy)
        (expect found :to-equal nil)
        (expect handler :to-equal nil)))))))))


;;; ─── Condition Construction ───────────────────────────────────────────────


;;; ─── Condition Hierarchy ──────────────────────────────────────────────────


;;; ─── Condition Report ─────────────────────────────────────────────────────


;;; ─── Handler Stack ────────────────────────────────────────────────────────


;;; ─── Restart Bindings ─────────────────────────────────────────────────────


;;; ─── Signal Dispatch ──────────────────────────────────────────────────────


;;; ─── Clear Context ────────────────────────────────────────────────────────

