(in-package #:cl-cc/vm/execution)

;;;; Continuation/trampoline machinery (vm-capture-continuation,
;;;; vm-invoke-continuation, vm-force-trampoline-result) and the 51-bit
;;;; fixnum/bignum/rational arithmetic helpers overflow-checked numeric
;;;; instructions dispatch through, plus those instructions'
;;;; execute-instruction methods.

(defstruct vm-trampoline-thunk
  "A VM-level zero-argument thunk for external trampoline dispatch."
  (function (lambda () nil) :type function))

(defstruct (vm-continuation (:constructor %make-vm-continuation))
  "Heap-copied VM continuation snapshot.

The register file, call stack, handler stack, method stack, prompt stack,
physical stack metadata, and closure environment are copied at capture time.
Invocation copies or restores each snapshot again, keeping continuations
multi-shot. KIND is :FULL, :ESCAPE, or :DELIMITED; escape continuations are
one-shot by convention."
  (kind :full :type keyword)
  pc
  dst-reg
  registers
  call-stack
  handler-stack
  method-call-stack
  prompt-stack
  stack-segment-snapshot
  closure-env
  values-list
  labels
  (used-p nil :type boolean))

(defun %vm-copy-register-table (table)
  (typecase table
    (hash-table
     (let ((copy (make-hash-table :test (hash-table-test table))))
       (maphash (lambda (key value) (setf (gethash key copy) value)) table)
       copy))
    (simple-vector
     (copy-seq table))
    (vector
     (copy-seq table))
    (t
     (error "Unsupported VM register file: ~S" table))))

(defun %vm-stack-segment-snapshot (state)
  (cl-cc/runtime::stack-segment-snapshot (vm-current-stack-segment state)))

(defun %vm-replace-stack-segments (state snapshot)
  "Replace STATE's chain only after SNAPSHOT has restored successfully."
  (let ((replacement (cl-cc/runtime::stack-segment-restore snapshot))
        (current (vm-current-stack-segment state)))
    (cl-cc/runtime::release-stack-segment-chain current)
    (setf (vm-current-stack-segment state) replacement)))

(defun vm-capture-continuation (state pc dst-reg &key (kind :full) prompt-frame labels)
  "Copy STATE's VM control stack to a reusable heap continuation object."
  (let* ((call-stack (copy-tree (vm-call-stack state)))
         (method-stack (copy-tree (vm-method-call-stack state)))
         (handler-stack (copy-tree (vm-handler-stack state)))
         (prompt-stack (copy-tree (vm-continuation-prompts state)))
         (stack-segment-snapshot
           (if prompt-frame
               (copy-tree (getf prompt-frame :stack-segment-snapshot))
               (%vm-stack-segment-snapshot state))))
    (when (and prompt-frame (member prompt-frame prompt-stack :test #'equal))
      (setf call-stack (copy-tree (getf prompt-frame :call-stack))
            method-stack (copy-tree (getf prompt-frame :method-call-stack))
            handler-stack (copy-tree (getf prompt-frame :handler-stack))))
    (%make-vm-continuation
     :kind kind
     :pc pc
     :dst-reg dst-reg
     :registers (%vm-copy-register-table (vm-state-registers state))
     :call-stack call-stack
     :handler-stack handler-stack
     :method-call-stack method-stack
     :prompt-stack prompt-stack
     :stack-segment-snapshot stack-segment-snapshot
     :closure-env (vm-closure-env state)
     :values-list (copy-list (vm-values-list state))
     :labels labels)))

(defun vm-invoke-continuation (state continuation value)
  "Restore CONTINUATION into STATE, store VALUE, and return the resume PC."
  (when (and (eq (vm-continuation-kind continuation) :escape)
             (vm-continuation-used-p continuation))
    (error "Escape continuation invoked more than once: ~S" continuation))
  (setf (vm-continuation-used-p continuation) t)
  (%vm-replace-stack-segments
   state (vm-continuation-stack-segment-snapshot continuation))
  (vm-restore-registers state (%vm-copy-register-table (vm-continuation-registers continuation)))
  (setf (vm-call-stack state) (copy-tree (vm-continuation-call-stack continuation))
        (vm-handler-stack state) (copy-tree (vm-continuation-handler-stack continuation))
        (vm-method-call-stack state) (copy-tree (vm-continuation-method-call-stack continuation))
        (vm-continuation-prompts state) (copy-tree (vm-continuation-prompt-stack continuation))
        (vm-closure-env state) (vm-continuation-closure-env continuation)
        (vm-values-list state) (copy-list (vm-continuation-values-list continuation))
        (vm-stack-depth state) (length (vm-continuation-call-stack continuation)))
  (vm-reg-set state (vm-continuation-dst-reg continuation) value)
  (vm-continuation-pc continuation))

(defun vm-force-trampoline-result (value)
  "Force VM trampoline thunks until VALUE is no longer a thunk."
  (loop for current = value then (funcall (vm-trampoline-thunk-function current))
        while (vm-trampoline-thunk-p current)
        finally (return current)))
