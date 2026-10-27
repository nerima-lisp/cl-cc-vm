(in-package #:cl-cc/vm/execution/test)

(defun register-vm-boundary-tests ()
  (describe-sequential "cl-cc-vm FASL serialization"
    (it "round-trips programs and load-time-value cells through explicit wire data"
      (let* ((unresolved
               (cl-cc/vm:make-vm-load-time-value-cell
                :id 7 :form (quote (+ 20 22)) :read-only-p t))
             (resolved-value (list :resolved 42))
             (resolved
               (cl-cc/vm:make-vm-load-time-value-cell
                :id 8 :resolved-p t :value resolved-value))
             (program
               (cl-cc/vm:make-vm-program
                :instructions
                (list (cl-cc/vm:make-vm-load-time-value :dst :r0 :cell-id 7)
                      (cl-cc/vm:make-vm-halt :reg :r0))
                :result-register :r0
                :leaf-p t
                :calling-convention :internal
                :function-conventions (quote ((helper . :internal)))
                :deopt-info (quote ((0 . checkpoint)))
                :osr-entry-points (quote ((loop . 1)))
                :tier1-entry-points (quote ((entry . 0)))
                :load-time-value-cells (list unresolved resolved)
                :compilation-tier 1))
             (wire
               (with-output-to-string (stream)
                 (cl-cc/vm:vm-write-to-fasl program stream)))
             (copy
               (with-input-from-string (stream wire)
                 (cl-cc/vm:vm-read-from-fasl stream)))
             (instructions (cl-cc/vm:vm-program-instructions copy))
             (cells (cl-cc/vm/execution::vm-program-load-time-value-cells copy))
             (unresolved-copy (first cells))
             (resolved-copy (second cells)))
        (expect (search "(:VM-FASL-PROGRAM" wire) :to-be-truthy)
        (expect (search "#S(VM-PROGRAM" wire) :to-be nil)
        (expect (typep copy (quote cl-cc/vm:vm-program)) :to-be-truthy)
        (expect (equal (mapcar (function cl-cc/vm:instruction->sexp) instructions) (quote ((:load-time-value :r0 7) (:halt :r0)))) :to-be-truthy)
        (expect (cl-cc/vm:vm-load-time-value-cell-id unresolved-copy) :to-be 7)
        (expect (equal (cl-cc/vm:vm-load-time-value-cell-form unresolved-copy) (quote (+ 20 22))) :to-be-truthy)
        (expect (cl-cc/vm:vm-load-time-value-cell-read-only-p unresolved-copy)
                :to-be-truthy)
        (expect (cl-cc/vm:vm-load-time-value-cell-resolved-p unresolved-copy)
                :to-be nil)
        (expect (cl-cc/vm:vm-load-time-value-cell-id resolved-copy) :to-be 8)
        (expect (cl-cc/vm:vm-load-time-value-cell-resolved-p resolved-copy)
                :to-be-truthy)
        (expect (equal (cl-cc/vm:vm-load-time-value-cell-value resolved-copy) resolved-value) :to-be-truthy)
        (expect (cl-cc/vm:vm-program-result-register copy) :to-be :r0)
        (expect (cl-cc/vm:vm-program-leaf-p copy) :to-be-truthy)
        (expect (cl-cc/vm:vm-program-calling-convention copy) :to-be :internal)
        (expect (equal (cl-cc/vm:vm-program-function-conventions copy) (quote ((helper . :internal)))) :to-be-truthy)
        (expect (equal (cl-cc/vm:vm-program-deopt-info copy) (quote ((0 . checkpoint)))) :to-be-truthy)
        (expect (equal (cl-cc/vm:vm-program-osr-entry-points copy) (quote ((loop . 1)))) :to-be-truthy)
        (expect (equal (cl-cc/vm:vm-program-tier1-entry-points copy) (quote ((entry . 0)))) :to-be-truthy)
        (expect (cl-cc/vm:vm-program-compilation-tier copy) :to-be 1))))
  (describe-sequential "continuation physical stack ownership"
    (it "restores an independently owned chain on every invocation"
      (let ((cl-cc/runtime::*stack-segment-pool* nil))
        (let* ((state (cl-cc/vm:make-vm-state))
               (captured-root (cl-cc/runtime::stack-segment-note-frame nil 128))
               (frame-bytes (cl-cc/runtime::stack-segment-size captured-root))
               (captured-current
                 (cl-cc/runtime::stack-segment-note-frame captured-root frame-bytes)))
          (setf (cl-cc/vm/execution::vm-current-stack-segment state) captured-current)
          (let ((continuation (cl-cc/vm:vm-capture-continuation state 17 :r0)))
            (cl-cc/vm:vm-invoke-continuation state continuation :first)
            (let ((first (cl-cc/vm/execution::vm-current-stack-segment state)))
              (expect (not (eq first captured-current)) :to-be-truthy)
              (expect (cl-cc/runtime::stack-segment-used captured-current) :to-be 0)
              (cl-cc/vm:vm-invoke-continuation state continuation :second)
              (let ((second (cl-cc/vm/execution::vm-current-stack-segment state)))
                (expect (not (eq second first)) :to-be-truthy)
                (expect (cl-cc/runtime::stack-segment-used first) :to-be 0)
                (expect (cl-cc/runtime::stack-segment-prev first) :to-be-null)
                (expect (cl-cc/runtime::stack-segment-used second) :to-be frame-bytes)
                (expect (cl-cc/runtime::stack-segment-used
                          (cl-cc/runtime::stack-segment-prev second))
                        :to-be 128)
                (cl-cc/runtime::release-stack-segment-chain second)
                (setf (cl-cc/vm/execution::vm-current-stack-segment state) nil)))))))))
