(in-package :cl-cc/vm)

(defmethod execute-instruction ((inst vm-osr-entry) state pc labels)
  (unless *osr-enabled*
    (return-from execute-instruction (values (1+ pc) nil nil)))
  ;; Phase 1: Look up current PC in osr-entry-points metadata
  (let ((entry (and (boundp '*vm-current-program-osr-entry-points*)
                    (symbol-value '*vm-current-program-osr-entry-points*)
                    (find pc *vm-current-program-osr-entry-points*
                          :key (lambda (e) (getf e :pc))
                          :test #'eql))))
    (when entry
      ;; Phase 2: If a deopt frame is pending from a prior compiled→interpreter
      ;; transition, reconstruct the interpreter state from its saved registers.
      (let ((deopt-frame (vm-current-deopt-frame state)))
        (when deopt-frame
          (vm-reconstruct-interpreter-frame state deopt-frame))))

    ;; Phase 3: Check for Tier-1 target PC and jump, or continue to next inst.
    (vm-osr-stub-enter state labels inst pc)))

(defmethod execute-instruction ((inst vm-print) state pc labels)
  (declare (ignore labels))
  (format (vm-output-stream state) "~A~%" (vm-reg-get state (vm-reg inst)))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-halt) state pc labels)
  (declare (ignore pc labels))
  (values nil t (vm-reg-get state (vm-reg inst))))

(defmethod execute-instruction ((inst vm-closure) state pc labels)
  (declare (ignore labels))
  (let* ((dst-reg (vm-dst inst))
          (captured-bindings (vm-captured-vars inst))
          (captured-regs (coerce (mapcar #'cdr captured-bindings) 'vector))
          (captured-vals (coerce (mapcar (lambda (binding)
                                           (vm-reg-get state (cdr binding)))
                                         captured-bindings)
                                 'vector))
           (closure (make-instance 'vm-closure-object
                                   :entry-label (vm-label-name inst)
                                   :params (vm-closure-params inst)
                                  :optional-params (vm-closure-optional-params inst)
                                  :rest-param (vm-closure-rest-param inst)
                                   :key-params (vm-closure-key-params inst)
                                   :rest-stack-alloc-p (vm-closure-rest-stack-alloc-p inst)
                                    :dispatch-tag (vm-closure-inst-dispatch-tag inst)
                                    :captured-regs captured-regs
                                    :captured-vals captured-vals
                                    :program-flat *vm-exec-flat*
                                    :label-table labels)))
    (vm-reg-set state dst-reg closure)
    ;; Fix self-references for recursive labels: if a captured register
    ;; is the same as dst-reg, the closure captures itself (not yet created at lookup time)
    (dotimes (i (length captured-regs))
      (when (eql (aref captured-regs i) dst-reg)
        (setf (aref captured-vals i) closure)))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-call) state pc labels)
  (let ((func (vm-resolve-function state (vm-reg-get state (vm-func-reg inst)))))
    (%vm-dispatch-call func state pc labels (vm-args inst) (vm-dst inst) nil
                       (vm-call-live-regs inst))))

(defmethod execute-instruction ((inst vm-tail-call) state pc labels)
  (let ((func (vm-resolve-function state (vm-reg-get state (vm-func-reg inst)))))
    (%vm-dispatch-call func state pc labels (vm-args inst) (vm-dst inst) t
                        (vm-tail-call-live-regs inst))))

(defmethod execute-instruction ((inst vm-call/cc) state pc labels)
  (let* ((func (vm-resolve-function state (vm-reg-get state (vm-func-reg inst))))
         (cont (vm-capture-continuation state (1+ pc) (vm-dst inst)
                                        :kind :full
                                        :labels labels))
         (arg-reg (gensym "CONT-REG")))
    (vm-reg-set state arg-reg cont)
    (%vm-dispatch-call func state pc labels (list arg-reg) (vm-dst inst) nil)))

(defmethod execute-instruction ((inst vm-call-with-prompt) state pc labels)
  (let* ((func (vm-resolve-function state (vm-reg-get state (vm-func-reg inst))))
         (prompt-name (vm-reg-get state (vm-prompt-reg inst)))
         (frame (list :name prompt-name
                      :pc (1+ pc)
                      :dst-reg (vm-dst inst)
                      :call-stack (copy-tree (vm-call-stack state))
                      :handler-stack (copy-tree (vm-handler-stack state))
                      :method-call-stack (copy-tree (vm-method-call-stack state))
                      :stack-segment-snapshot (%vm-stack-segment-snapshot state))))
    (push frame (vm-continuation-prompts state))
    (%vm-dispatch-call func state pc labels nil (vm-dst inst) nil)))

(defmethod execute-instruction ((inst vm-abort-to-prompt) state pc labels)
  (declare (ignore pc labels))
  (let* ((prompt-name (vm-reg-get state (vm-prompt-reg inst)))
         (value (vm-reg-get state (vm-value-reg inst)))
         (frame (find prompt-name (vm-continuation-prompts state)
                      :key (lambda (entry) (getf entry :name))
                      :test #'equal)))
    (unless frame
      (error "No continuation prompt named ~S is active" prompt-name))
    (%vm-replace-stack-segments state (getf frame :stack-segment-snapshot))
    (setf (vm-call-stack state) (copy-tree (getf frame :call-stack))
          (vm-handler-stack state) (copy-tree (getf frame :handler-stack))
          (vm-method-call-stack state) (copy-tree (getf frame :method-call-stack))
          (vm-stack-depth state) (length (getf frame :call-stack)))
    (vm-reg-set state (getf frame :dst-reg) value)
    (setf (vm-continuation-prompts state)
          (rest (member frame (vm-continuation-prompts state) :test #'eq)))
    (values (getf frame :pc) nil nil)))

(defmethod execute-instruction ((inst vm-trampoline) state pc labels)
  (declare (ignore labels))
  (let* ((func (vm-resolve-function state (vm-reg-get state (vm-func-reg inst))))
         (arg-values (mapcar (lambda (reg) (vm-reg-get state reg)) (vm-args inst))))
    (vm-reg-set state (vm-dst inst)
                (make-vm-trampoline-thunk
                 :function
                 (lambda ()
                   (cond
                     ((functionp func) (apply func arg-values))
                     ((typep func 'vm-closure-object)
                      (%vm-call-closure-sync func state arg-values))
                      (t (error "Cannot trampoline function designator: ~S" func))))))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-recompile) state pc labels)
  (declare (ignore labels))
  (let ((func (vm-resolve-function state (vm-reg-get state (vm-func-reg inst)))))
    (unless (typep func 'vm-closure-object)
      (error "Cannot recompile non-closure function: ~S" func))
    (vm-maybe-tier-upgrade-closure func (vm-recompile-tier inst))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-ret) state pc labels)
  (declare (ignore pc))
  (let ((result (vm-reg-get state (vm-reg inst))))
    (when (zerop (vm-mv-count state))
      (vm-store-multiple-values state (list result)))
    ;; Check for qualified method dispatch (standard method combination)
    (let ((method-entry (car (vm-method-call-stack state))))
      (when (and method-entry (listp method-entry)
                 (getf (cdddr method-entry) :qualified))
        ;; We're returning from a qualified method — continue the chain
        (return-from execute-instruction
          (%vm-ret-qualified-dispatch state result labels method-entry))))
    ;; Normal return path
    (if (vm-call-stack state)
        (destructuring-bind (return-pc dst-reg old-closure-env saved-regs &optional saved-mv-buffer saved-mv-count)
            (vm-pop-call-frame state)
          (declare (ignore saved-mv-buffer saved-mv-count))
          ;; Pop method-call-stack in sync with call-stack
          (when (vm-method-call-stack state)
            (let ((method-frame (pop (vm-method-call-stack state))))
              (when (and method-frame (listp method-frame)
                         (getf (cdddr method-frame) :combination)
                         (vm-clos-shadow-stack state))
                (pop (vm-clos-shadow-stack state)))
              (when (and method-frame (listp method-frame)
                         (getf (cdddr method-frame) :shadow)
                         (vm-clos-shadow-stack state))
                (pop (vm-clos-shadow-stack state)))))
          (vm-verify-stack-canary saved-regs)
          (vm-profile-return state)
          (vm-restore-registers state saved-regs)
          ;; Write the return value into the destination register
           (vm-reg-set state dst-reg result)
          (when (and (vm-continuation-prompts state)
                     (eql (getf (first (vm-continuation-prompts state)) :pc) return-pc)
                     (eql (getf (first (vm-continuation-prompts state)) :dst-reg) dst-reg))
            (pop (vm-continuation-prompts state)))
           (when old-closure-env
             (setf (vm-closure-env state) old-closure-env))
          (values return-pc nil nil))
        (values nil t result))))

(defmethod execute-instruction ((inst vm-func-ref) state pc labels)
  (declare (ignore labels))
  ;; Resolve compiled definitions before host bridges, preserving CL-CC overrides.
  (labels ((symbol-candidates (name)
             (remove nil
                     (list (find-symbol name :cl-cc)
                           (find-symbol name :cl))
                     :test #'eq))
           (registered-callable (candidates)
             (dolist (candidate candidates)
               (let ((entry (vm-instance-function-value state candidate)))
                 (when (cl-cc/vm::%vm-callable-registry-entry-p entry)
                   (return entry)))))
           (bridge-callable (candidates)
             (dolist (candidate candidates)
               (let ((callable (vm-bridge-callable candidate)))
                 (when callable
                   (return callable))))))
    (let* ((label-str (vm-label-name inst))
           (candidates (symbol-candidates label-str)))
      (vm-reg-set state (vm-dst inst)
                  (or (registered-callable candidates)
                      (bridge-callable candidates)
                      (make-instance 'vm-closure-object
                                     :entry-label label-str
                                     :params (vm-closure-params inst)
                                     :optional-params (vm-closure-optional-params inst)
                                     :rest-param (vm-closure-rest-param inst)
                                     :key-params (vm-closure-key-params inst)
                                     :rest-stack-alloc-p (vm-closure-rest-stack-alloc-p inst)
                                     :dispatch-tag (vm-func-ref-dispatch-tag inst)
                                     :captured-regs #()
                                     :captured-vals #()
                                     :program-flat *vm-exec-flat*
                                     :label-table labels)))))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-make-closure) state pc labels)
  (declare (ignore labels))
  (let* ((captured-vals (coerce (mapcar (lambda (reg) (vm-reg-get state reg)) (vm-env-regs inst)) 'vector))
           (closure (make-instance 'vm-closure-object
                                   :entry-label (vm-label-name inst)
                                   :params (vm-make-closure-params inst)
                                    :rest-stack-alloc-p nil
                                    :captured-regs #()
                                    :captured-vals captured-vals
                                    :program-flat *vm-exec-flat*
                                    :label-table labels))
         (addr (vm-heap-alloc state closure)))
    (vm-reg-set state (vm-dst inst) addr)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-closure-ref-idx) state pc labels)
  (declare (ignore labels))
  (let* ((addr (vm-reg-get state (vm-closure-reg inst)))
         (closure (vm-heap-get state addr))
         (idx (vm-closure-index inst))
          (values-vec (vm-closure-captured-vals closure)))
    (when (>= idx (length values-vec))
      (error "Closure ref index ~D out of bounds (captured ~D values)" idx (length values-vec)))
    (vm-reg-set state (vm-dst inst) (aref values-vec idx))
    (values (1+ pc) nil nil)))
