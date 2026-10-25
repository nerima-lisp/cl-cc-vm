(in-package :cl-cc/vm)

;;; ─── VM Handler Stack ────────────────────────────────────────────────────────
;;;
;;; Since we cannot modify vm-state, we use a hash table to associate
;;; handler stacks with VM states. This is managed by the VM condition
;;; instructions.

(defvar *vm-handler-stacks* (make-hash-table :test #'eq :weakness :key)
  "Hash table mapping VM states to their handler stacks.
Uses weak keys so handlers are GC'd when the VM state is collected.")

(defvar *vm-restart-bindings* (make-hash-table :test #'eq :weakness :key)
  "Hash table mapping VM states to their restart bindings.
Uses weak keys for the same reason as *vm-handler-stacks*.")

;;; Structure representing a condition handler in the VM.
;;; TYPE - Condition type this handler matches.
;;; HANDLER-FN - Function to call when condition is signaled.
(defstruct (vm-handler (:constructor make-vm-handler (type handler-fn)))
  type
  handler-fn)

;;; Structure representing a restart in the VM.
;;; NAME - Name of the restart.
;;; RESTART-FN - Function to invoke the restart.
(defstruct (vm-restart (:constructor make-vm-restart (name restart-fn)))
  name
  restart-fn
  (description nil)
  (interactive-function nil))

(defun describe-restart (restart)
  "Return a human-readable description for RESTART."
  (cond ((typep restart 'vm-restart)
         (or (vm-restart-description restart)
             (format nil "Invoke restart ~S" (vm-restart-name restart))))
        ((ignore-errors (restart-name restart))
         (with-output-to-string (out)
           (ignore-errors (princ restart out))))
        (t (format nil "Invoke restart ~S" restart))))

(defun restart-interactive (restart)
  "Return RESTART's interactive argument collection function, if any."
  (cond ((typep restart 'vm-restart) (vm-restart-interactive-function restart))
        ((ignore-errors (restart-name restart))
         (ignore-errors (restart-interactive-function restart)))
        (t nil)))

(defun vm-compute-active-restarts (&optional condition vm-state)
  "Return active VM and host restarts for CONDITION."
  (append *active-restarts*
          (and vm-state (vm-get-restarts vm-state))
          (cl:compute-restarts condition)))

(defun vm-invoke-restart-interactively (restart)
  "Collect interactive arguments for RESTART, then invoke it."
  (let ((interactive (restart-interactive restart)))
    (cond ((typep restart 'vm-restart)
           (apply (vm-restart-restart-fn restart)
                  (if interactive (funcall interactive) nil)))
          (interactive
           (apply #'cl:invoke-restart restart (funcall interactive)))
          (t (cl:invoke-restart-interactively restart)))))

(defun vm-show-restart-menu (condition &optional (stream *query-io*))
  "Display a numbered restart menu for CONDITION and return selected restart."
  (let ((restarts (vm-compute-active-restarts condition)))
    (format stream "~&Debugger entered on ~A~%" condition)
    (loop for restart in restarts
          for index from 0
          do (format stream "  ~D: [~A] ~A~%" index
                     (if (typep restart 'vm-restart)
                         (vm-restart-name restart)
                         (restart-name restart))
                     (describe-restart restart)))
    (when restarts
      (format stream "Select restart number: ")
      (finish-output stream)
      (let ((choice (ignore-errors (read stream nil nil))))
        (when (and (integerp choice) (<= 0 choice) (< choice (length restarts)))
          (nth choice restarts))))))

(defmacro vm-with-simple-restart ((name format-string &rest args) &body body)
  "VM-prefixed wrapper for CL:WITH-SIMPLE-RESTART."
  `(cl:with-simple-restart (,name ,format-string ,@args) ,@body))

(defun vm-get-handler-stack (vm-state)
  "Get the handler stack for VM-STATE, creating one if necessary."
  (or (gethash vm-state *vm-handler-stacks*)
      (setf (gethash vm-state *vm-handler-stacks*) nil)))

(defun vm-push-handler-to-stack (vm-state type handler-fn)
  "Push a new handler onto the handler stack for VM-STATE."
  (push (make-vm-handler type handler-fn)
        (gethash vm-state *vm-handler-stacks*)))

(defun vm-pop-handler-from-stack (vm-state)
  "Pop the top handler from the handler stack for VM-STATE.
Returns the popped handler or NIL if stack is empty."
  (let ((stack (vm-get-handler-stack vm-state)))
    (when stack
      (let ((handler (pop stack)))
        (setf (gethash vm-state *vm-handler-stacks*) stack)
        handler))))

(defun vm-find-handler (vm-state condition)
  "Find a handler for CONDITION in VM-STATE's handler stack.
Returns the first matching handler or NIL if none found."
  (let ((stack (vm-get-handler-stack vm-state)))
    (find-if (lambda (handler)
               (typep condition (vm-handler-type handler)))
             stack)))

(defun vm-get-restarts (vm-state)
  "Get the available restarts for VM-STATE."
  (gethash vm-state *vm-restart-bindings*))

(defun vm-add-restart (vm-state name restart-fn)
  "Add a restart binding for VM-STATE."
  (push (make-vm-restart name restart-fn)
        (gethash vm-state *vm-restart-bindings*)))

(defun vm-find-restart (vm-state name)
  "Find a restart by name in VM-STATE's restart bindings."
  (let ((restarts (vm-get-restarts vm-state)))
    (find name restarts :key #'vm-restart-name :test #'eq)))

(defun vm-clear-condition-context (vm-state)
  "Clear handler stack and restarts for VM-STATE (cleanup function)."
  (remhash vm-state *vm-handler-stacks*)
  (remhash vm-state *vm-restart-bindings*))
