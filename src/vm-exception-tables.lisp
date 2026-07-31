(in-package :cl-cc/vm)

;;; ─── Zero-Cost Exception Tables ─────────────────────────────────────────────
;;;
;;; FR-138 stores handler metadata outside the hot instruction stream.  The
;;; compiler registers a vector of PC ranges for each VM program; label-table
;;; construction then associates the freshly built label table with that vector
;;; so condition signaling can find a handler from the current PC without
;;; executing establish/remove instructions on protected-form entry/exit.

(defstruct (vm-exception-entry
            (:constructor make-vm-exception-entry
                (start-pc end-pc handler-pc condition-type result-reg order)))
  start-pc
  end-pc
  handler-pc
  condition-type
  result-reg
  order)

(defvar *vm-program-exception-tables* (make-hash-table :test #'eq :weakness :key)
  "Weak map from VM program objects to their zero-cost exception table.")

(defvar *vm-instruction-exception-tables* (make-hash-table :test #'eq :weakness :key)
  "Weak map from VM program instruction lists to their exception table.")

(defvar *vm-label-exception-tables* (make-hash-table :test #'eq :weakness :key)
  "Weak map from per-run label tables to their exception table.")

(defun vm-register-program-exception-table (program exception-table)
  "Attach EXCEPTION-TABLE to PROGRAM and its instruction list.

EXCEPTION-TABLE is a vector of VM-EXCEPTION-ENTRY records with concrete PCs.
An empty table is ignored so programs without handlers keep the old shape."
  (let ((table (and exception-table
                    (if (vectorp exception-table)
                        exception-table
                        (coerce exception-table 'vector)))))
    (when (and program table (plusp (length table)))
      (setf (gethash program *vm-program-exception-tables*) table)
      (setf (gethash (vm-program-instructions program) *vm-instruction-exception-tables*) table))
    program))

(defun %vm-exception-table-for-labels (labels)
  "Return the exception table associated with LABELS, if any."
  (and labels (gethash labels *vm-label-exception-tables*)))

(defun %vm-exception-entry-active-p (entry pc condition)
  "Return true when ENTRY covers PC and accepts CONDITION."
  (and (<= (vm-exception-entry-start-pc entry) pc)
       (< pc (vm-exception-entry-end-pc entry))
       (vm-error-type-matches-p condition (vm-exception-entry-condition-type entry))))

(defun %vm-exception-entry-span (entry)
  (- (vm-exception-entry-end-pc entry)
     (vm-exception-entry-start-pc entry)))

(defun vm-find-exception-table-entry (labels pc condition)
  "Find the innermost exception-table handler for CONDITION at PC.

For identical ranges, preserve source clause order.  For nested protected
forms, prefer the smallest covering range."
  (let ((best nil))
    (let ((table (%vm-exception-table-for-labels labels)))
      (when table
        (loop for entry across table
              when (%vm-exception-entry-active-p entry pc condition)
                do (when (or (null best)
                             (< (%vm-exception-entry-span entry)
                                (%vm-exception-entry-span best))
                             (and (= (%vm-exception-entry-span entry)
                                     (%vm-exception-entry-span best))
                                  (< (vm-exception-entry-order entry)
                                     (vm-exception-entry-order best))))
                     (setf best entry)))))
    best))

(defun %vm-jump-to-exception-entry (state entry condition)
  "Store CONDITION for ENTRY's handler and jump to its handler PC."
  (vm-reg-set state (vm-exception-entry-result-reg entry) condition)
  (values (vm-exception-entry-handler-pc entry) nil nil))

(defun %vm-restore-frame-for-exception-propagation (state frame)
  "Restore one caller frame while propagating an exception-table lookup."
  (destructuring-bind (return-pc _dst-reg old-closure-env saved-regs &rest _extra) frame
    (declare (ignore _dst-reg _extra))
    (vm-restore-registers state saved-regs)
    (when old-closure-env
      (setf (vm-closure-env state) old-closure-env))
    (when (vm-method-call-stack state)
      (pop (vm-method-call-stack state)))
    return-pc))

(defun vm-dispatch-exception-table (state labels pc condition &key error-p)
  "Resolve CONDITION via the PC→handler side table, unwinding callers as needed.

Returns normal EXECUTE-INSTRUCTION values when a handler is found.  If no entry
matches in the current frame, restore one call frame and retry at its return PC;
this models dynamic propagation without a runtime handler stack."
  (if (%vm-exception-table-for-labels labels)
      (loop with probe-pc = pc
            do (let ((entry (vm-find-exception-table-entry labels probe-pc condition)))
                 (when entry
                   (return (%vm-jump-to-exception-entry state entry condition)))
                 (if (vm-call-stack state)
                     ;; The popped frame's return PC points just past the call
                     ;; instruction; probe at the call site itself (return-pc - 1)
                     ;; so a protected range covering the call still matches.
                     (setf probe-pc
                           (1- (%vm-restore-frame-for-exception-propagation
                                state (vm-pop-call-frame state))))
                     (progn
                      (when error-p
                        (unless (typep condition 'vm-fatal-error)
                          (vm-print-backtrace state :labels labels))
                        (if (typep condition 'condition)
                            (error condition)
                            (error "Unhandled error in VM: ~S" condition)))
                       (return (values nil nil nil))))))
      (values nil nil nil)))

(defun build-label-table (instructions)
  "Build an integer-keyed label table and associate any exception side table."
  (let ((labels (make-hash-table :test #'eql)))
    (loop for inst across (coerce instructions 'vector)
          for pc from 0
          do (when (typep inst 'vm-label)
               (vm-label-table-store labels (vm-name inst) pc)))
    (let ((exception-table (gethash instructions *vm-instruction-exception-tables*)))
      (when exception-table
        (setf (gethash labels *vm-label-exception-tables*) exception-table)))
    labels))

(defun %vm-stack-handler-nested-inside-table-p (state labels pc error-value)
  "Return T when a matching handler-stack entry is nested inside the exception
table entry that would otherwise take this condition.

cl-cc has two handler mechanisms: HANDLER-CASE registers PC ranges in the
zero-cost exception table, while UNWIND-PROTECT pushes onto the handler stack
(VM-ESTABLISH-HANDLER is emitted only by COMPILE-AST for AST-UNWIND-PROTECT).
Consulting the table first regardless of nesting sent every error inside
(handler-case (unwind-protect ... cleanup) ...) straight to the outer
HANDLER-CASE, so the cleanup never ran — the one thing UNWIND-PROTECT exists to
guarantee. A stack handler established at a PC within the table entry's protected
range is the inner one and must win."
  (let ((entry (and (%vm-exception-table-for-labels labels)
                    (vm-find-exception-table-entry labels pc error-value))))
    (when entry
      (let ((start (vm-exception-entry-start-pc entry))
            (end   (vm-exception-entry-end-pc entry)))
        (dolist (handler (vm-handler-stack state) nil)
          (let ((established-pc (seventh handler)))
            (when (and (vm-error-type-matches-p error-value (third handler))
                       (integerp established-pc)
                       (<= start established-pc)
                       (<= established-pc end))
              (return t))))))))

(defun %vm-run-handler-bind-handlers (state error-value)
  "Offer ERROR-VALUE to the guest's HANDLER-BIND handlers before unwinding.

HANDLER-BIND pushes (type function) entries onto the guest special
*%CONDITION-HANDLERS*, and only the SIGNAL macro walked that list — so a
HANDLER-BIND handler never ran for an ERROR, which is most of what ANSI uses
HANDLER-BIND for, and is why ASSERT's STORE-VALUE protocol could not work: the
handler that calls STORE-VALUE was never entered.

A handler that returns declines and we fall through to the usual dispatch; one
that transfers control (INVOKE-RESTART, THROW, RETURN-FROM) never comes back. The
list is read by symbol *name*: it is bound under a :CL-CC symbol while this code
names it from :CL-CC/VM."
  (let ((handlers nil))
    ;; Several same-named symbols can be present — the pre-seeded
    ;; *VM-INITIAL-GLOBALS* entry, the stdlib DEFVAR, and whichever one PROGV
    ;; actually bound — and only one of them holds the live list. Take the first
    ;; non-empty one rather than the first match, which is hash-order dependent.
    (maphash (lambda (key value)
               (when (and (null handlers)
                          (symbolp key)
                          (string= (symbol-name key) "*%CONDITION-HANDLERS*")
                          (consp value))
                 (setf handlers value)))
             (vm-global-vars state))
    (when (listp handlers)
      (dolist (entry handlers)
        (when (and (consp entry) (consp (cdr entry)))
          (let ((type (first entry))
                (function (second entry)))
            (when (and function (vm-error-type-matches-p error-value type))
              (%vm-call-closure-sync function state (list error-value)))))))))

(defun %vm-signal-condition (state labels pc error-value)
  "Route ERROR-VALUE through the zero-cost exception table, then the legacy
handler stack.  Returns EXECUTE-INSTRUCTION values transferring control to the
matching guest handler.  When no handler matches, prints a VM backtrace and
raises, exactly as the VM-SIGNAL-ERROR opcode has always done."
  (%vm-run-handler-bind-handlers state error-value)
  (multiple-value-bind (next-pc handled-p value)
      (if (%vm-stack-handler-nested-inside-table-p state labels pc error-value)
          (values nil nil nil)
          (vm-dispatch-exception-table state labels pc error-value :error-p nil))
    (declare (ignore value))
    (if next-pc
        (values next-pc handled-p nil)
        (let ((matching-handler nil)
              (handlers-to-skip 0))
          (dolist (entry (vm-handler-stack state))
            (if (vm-error-type-matches-p error-value (third entry))
                (progn (setf matching-handler entry) (return))
                (incf handlers-to-skip)))
          (if matching-handler
              (progn
                (dotimes (i (1+ handlers-to-skip)) (pop (vm-handler-stack state)))
                (destructuring-bind (handler-label result-reg _type saved-call-stack saved-regs
                                     &optional saved-method-call-stack established-pc)
                    matching-handler
                  (declare (ignore _type established-pc))
                  (%vm-unwind-to-handler state labels handler-label result-reg
                                         saved-call-stack saved-regs saved-method-call-stack
                                         error-value)))
              (progn
                (unless (typep error-value 'vm-fatal-error)
                  (vm-print-backtrace state :labels labels))
                (if (typep error-value 'condition)
                    (error error-value)
                    (error "Unhandled error in VM: ~S" error-value))))))))

(defmethod execute-instruction ((inst vm-signal-error) state pc labels)
  "Signal an error using the zero-cost exception table before legacy stacks."
  (%vm-signal-condition state labels pc (vm-reg-get state (vm-error-reg inst))))

;;; ── Host-error routing into the guest condition system ──────────────────
;;;
;;; A guest program may trigger a *host* Lisp error while an instruction runs
;;; (e.g. calling an undefined function, or a host TYPE-ERROR from a builtin).
;;; ANSI CL requires such an error to be catchable by an enclosing guest
;;; HANDLER-CASE/HANDLER-BIND.  Only the VM-SIGNAL-ERROR opcode consulted the
;;; guest handler system, so host-raised conditions escaped uncaught.  The run
;;; loop wraps each instruction with VM-EXECUTE-INSTRUCTION-GUARDED, which — when
;;; a guest handler is actually in scope — offers a routable host condition to
;;; the very same dispatch path the opcode uses.

(defun %vm-host-error-routable-p (condition)
  "Return T when a host-raised CONDITION should be offered to the guest condition
system.  Excludes VM infrastructure guards that guest HANDLER-CASE must not
swallow (stack-overflow, evaluation deadline)."
  (and (typep condition 'error)
       (not (typep condition 'vm-stack-overflow))
       (not (typep condition 'vm-eval-deadline-exceeded))))

(defun %vm-exception-table-would-handle-p (state labels pc condition)
  "Non-destructively test whether the PC→handler side table has a live entry for
CONDITION at PC or at any caller frame's return PC."
  (and (%vm-exception-table-for-labels labels)
       (or (vm-find-exception-table-entry labels pc condition)
           ;; Probe each caller at its call site (return-pc - 1), matching the
           ;; unwinding walk in VM-DISPATCH-EXCEPTION-TABLE.
           (loop for frame in (vm-call-stack state)
                 thereis (vm-find-exception-table-entry labels (1- (first frame)) condition)))))

(defun %vm-guest-handler-exists-p (state labels pc condition)
  "Non-destructively test whether a guest handler (exception table or legacy
handler stack) would catch CONDITION signalled at PC."
  (or (%vm-exception-table-would-handle-p state labels pc condition)
      (some (lambda (entry)
              (and (not (eq (first entry) :catch))
                   (vm-error-type-matches-p condition (third entry))))
            (vm-handler-stack state))))

(defun %vm-guest-handlers-active-p (state labels)
  "Return T when any guest condition handler could be in scope for STATE."
  (or (vm-handler-stack state)
      (%vm-exception-table-for-labels labels)))

(defun vm-execute-instruction-guarded (instruction state pc labels)
  "Execute INSTRUCTION, routing a host-raised CL:ERROR into the guest condition
system so a compiled HANDLER-CASE/HANDLER-BIND catches it exactly as a guest
ERROR call would.  When no guest handler is in scope, or none matches, the host
condition propagates unchanged.  Returns the same values as EXECUTE-INSTRUCTION."
  (if (%vm-guest-handlers-active-p state labels)
      (let ((routed nil))
        (multiple-value-bind (next-pc halted value)
            (block done
              (handler-bind
                  ((error
                     (lambda (condition)
                       (when (and (%vm-host-error-routable-p condition)
                                  (%vm-guest-handler-exists-p state labels pc condition))
                         (setf routed condition)
                         (return-from done)))))
                (execute-instruction instruction state pc labels)))
          (if routed
              (%vm-signal-condition state labels pc routed)
              (values next-pc halted value))))
      (execute-instruction instruction state pc labels)))
