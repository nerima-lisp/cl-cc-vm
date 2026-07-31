(in-package :cl-cc/vm)

;;; FR-155: Deoptimization checkpoints and OSR markers.

(defun %vm-current-deopt-info (pc)
  "Return deoptimization metadata for PC from the active program, when present."
  (let ((program-info (and (boundp '*vm-current-program-deopt-info*)
                           (symbol-value '*vm-current-program-deopt-info*))))
    (cond
      ((hash-table-p program-info) (gethash pc program-info))
      ((listp program-info) (cdr (assoc pc program-info :test #'eql)))
      (t nil))))

(defun %vm-deopt-resume-pc (labels label pc)
  "Resolve a deoptimization resume LABEL, falling back to the next instruction."
  (or (and label (vm-label-table-lookup labels label))
      (1+ pc)))

(defun vm-capture-deopt-frame (state pc reason &optional info)
  "Capture enough VM state to rebuild an interpreter frame after deoptimization."
  (let* ((registers (vm-save-registers state))
         (vreg->preg (and info (vm-deopt-info-vreg->preg info)))
         (physical (and vreg->preg
                        (loop for (vreg . preg) in vreg->preg
                              collect (cons preg (gethash vreg registers))))))
    (make-vm-deopt-frame
     :pc pc
     :reason reason
     :registers registers
     :physical-registers physical
     :call-stack (copy-tree (vm-call-stack state))
     :closure-env (vm-closure-env state)
     :values-list (copy-list (vm-values-list state))
     :inline-stack (and info (copy-tree (vm-deopt-info-inline-stack info)))
     :info info)))

(defun vm-reconstruct-interpreter-frame (state frame)
  "Rebuild STATE from a saved FR-522 deoptimization FRAME."
  (when frame
    (when (vm-deopt-frame-registers frame)
      (vm-restore-registers state (vm-deopt-frame-registers frame)))
    (when (vm-deopt-frame-call-stack frame)
      (setf (vm-call-stack state) (vm-deopt-frame-call-stack frame)))
    (when (vm-deopt-frame-closure-env frame)
      (setf (vm-closure-env state) (vm-deopt-frame-closure-env frame)))
    (when (vm-deopt-frame-values-list frame)
      (setf (vm-values-list state) (vm-deopt-frame-values-list frame)))
    (setf (vm-current-deopt-frame state) nil)
    state))

(defun vm-trigger-deopt (state pc labels label reason)
  "Save register/interpreter state and resume at LABEL in the VM interpreter."
  (when *deopt-enabled*
    (let* ((info (%vm-current-deopt-info pc))
           (frame (vm-capture-deopt-frame state pc reason info)))
      (setf (vm-current-deopt-frame state) frame)
      (push frame (vm-deopt-history state))))
  (values (%vm-deopt-resume-pc labels label pc) nil nil))

(defmethod execute-instruction ((inst vm-type-check) state pc labels)
  (let ((value (vm-reg-get state (vm-src inst))))
    (if (vm-typep-check value (vm-type-name inst))
        (values (1+ pc) nil nil)
        (vm-trigger-deopt state pc labels (vm-type-check-deopt-label inst)
                          (list :type-check-failed
                                :register (vm-src inst)
                                :expected (vm-type-name inst)
                                :actual value
                                :id (vm-type-check-deopt-id inst))))))

(defmethod execute-instruction ((inst vm-deopt) state pc labels)
  (vm-trigger-deopt state pc labels (vm-deopt-label inst)
                    (list :deopt (vm-deopt-reason inst) :id (vm-deopt-id inst))))

(defun vm-register-tier1-osr-entry (state id entry)
  "Register simple Tier-1 OSR ENTRY metadata for ID.
ENTRY may be a PC integer, a label, or a plist containing :PC or :LABEL."
  (setf (gethash id (vm-tier1-code state)) entry))

(defun vm-osr-register-map (state &optional live-regs)
  "Return an interpreter virtual-register -> JIT physical-register map.
This infrastructure-level mapping is deterministic and portable; backend-specific
lowering can replace the synthetic physical names later."
  (let ((regs (or live-regs
                  (loop for reg being the hash-keys of (vm-state-registers state)
                        collect reg))))
    (loop for reg in regs
          for index from 0
          collect (cons reg (intern (format nil "P~D" index) :keyword)))))

(defun vm-materialize-osr-frame (state &key pc label id live-regs)
  "Save interpreter state to an OSR stack frame before entering Tier-1 code."
  (list :pc pc
        :label label
        :id id
        :vreg->preg (vm-osr-register-map state live-regs)
        :registers (vm-save-registers state)
        :call-stack (copy-tree (vm-call-stack state))
        :closure-env (vm-closure-env state)
        :values-list (copy-list (vm-values-list state))))

(defun vm-compile-osr-entry-if-hot (state id label pc)
  "Compile/register a conservative OSR entry when the loop header becomes hot.
The hook accepts the current STATE, OSR ID, loop LABEL, and interpreter PC.  If no
hook is installed, this records no target and execution remains in the interpreter."
  (when (and *osr-enabled* *vm-recompile-function-hook*)
    (let ((entry (funcall *vm-recompile-function-hook*
                          (list :osr id :label label :pc pc :state state)
                          1)))
      (when entry
        (vm-register-tier1-osr-entry state id entry)))))

(defun vm-osr-stub-enter (state labels inst pc)
  "FR-521 OSR stub: save interpreter state, enter Tier-1 target, restore on return."
  (let* ((frame (vm-materialize-osr-frame state
                                          :pc pc
                                          :label (vm-osr-label inst)
                                          :id (vm-osr-id inst)))
         (target-pc (or (%vm-tier1-osr-target-pc state labels (vm-osr-id inst) (vm-osr-label inst))
                        (progn
                          (vm-compile-osr-entry-if-hot state (vm-osr-id inst) (vm-osr-label inst) pc)
                          (%vm-tier1-osr-target-pc state labels (vm-osr-id inst) (vm-osr-label inst))))))
    (if target-pc
        (progn
          ;; The OSR frame is kept out of VM-CALL-STACK so normal RET frame
          ;; destructuring remains unchanged; deopt/re-entry can consume it via
          ;; VM-CURRENT-DEOPT-FRAME if the Tier-1 path returns to the interpreter.
          (setf (vm-current-deopt-frame state)
                (make-vm-deopt-frame :pc pc
                                      :reason :osr-entry
                                      :registers (getf frame :registers)
                                      :call-stack (getf frame :call-stack)
                                      :closure-env (getf frame :closure-env)
                                      :values-list (getf frame :values-list)
                                      :info frame))
          (values target-pc nil nil))
        (values (1+ pc) nil nil))))

(defun %vm-tier1-osr-target-pc (state labels id label)
  "Return a Tier-1 OSR target PC for ID/LABEL, or NIL when not ready."
  (let ((entry (or (gethash id (vm-tier1-code state))
                   (and label (gethash label (vm-tier1-code state))))))
    (cond
      ((integerp entry) entry)
      ((and (symbolp entry) labels) (vm-label-table-lookup labels entry))
      ((and (stringp entry) labels) (vm-label-table-lookup labels entry))
      ((and (consp entry) (getf entry :pc)) (getf entry :pc))
      ((and (consp entry) (getf entry :label))
       (vm-label-table-lookup labels (getf entry :label)))
      (t nil))))
