(in-package #:cl-cc/vm/execution)

(defun call-with-current-continuation (thunk)
  "FR-800: Call THUNK with a heap-copied VM continuation.

When invoked from the VM host bridge this captures the current VM register file,
call stack, handler stack, method stack, prompt stack, value buffer, resume PC,
and label table.  The captured continuation is a first-class VM object and may
be invoked more than once; each invocation restores a fresh copy of the saved
mutable control-state containers."
  (let ((state *vm-current-state*))
    (unless state
      (error "CALL-WITH-CURRENT-CONTINUATION requires an active VM state"))
    (let ((continuation (vm-capture-continuation state
                                                (or *vm-current-pc* 0)
                                                *vm-current-dst-reg*
                                                :kind :full
                                                :labels *vm-current-labels*)))
      (cond
        ((typep thunk 'vm-closure-object)
         (%vm-call-closure-sync thunk state (list continuation)))
        ((functionp thunk)
         (funcall thunk continuation))
        (t
         (error "CALL-WITH-CURRENT-CONTINUATION expected a function, got ~S" thunk))))))

(defun vm-call/cc (thunk)
  "FR-801: VM-level call-with-current-continuation."
  (call-with-current-continuation thunk))

(vm-register-host-bridge 'call-with-current-continuation #'call-with-current-continuation)

(vm-register-host-bridge 'vm-call/cc #'vm-call/cc)
