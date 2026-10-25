(in-package :cl-cc/vm)

;;; The VM condition type hierarchy (mirroring ANSI CL's condition types)
;;; and its PRINT-OBJECT report formatting. The handler/restart stack
;;; protocol built on these types is in condition-handler-stack.lisp; the
;;; VM bytecode-level exception-table dispatch (try/catch) is in
;;; vm-exception-tables.lisp (both load after this file).

(define-condition vm-condition (condition)
  ((vm-state :initarg :vm-state :reader vm-condition-state
              :documentation "The VM state when condition was signaled.")
   (error-code :initarg :error-code :initform nil :reader vm-condition-error-code
               :documentation "Machine-readable diagnostic code for this VM condition.")
   (vm-fix-it :initarg :fix-it :initform nil :reader vm-condition-fix-it
              :documentation "Optional structured fix-it suggestion for this VM condition.")
   (source-location :initarg :source-location :initform nil :reader %vm-condition-source-location
                    :documentation "Optional source location for rich error reports.")
   (source-text :initarg :source-text :initform nil :reader %vm-condition-source-text
                :documentation "Optional source text used for context-line display.")
   (suggestions :initarg :suggestions :initform nil :reader %vm-condition-suggestions
                :documentation "Optional did-you-mean suggestions."))
  (:documentation "Base class for all VM conditions."))

(define-condition vm-serious-condition (vm-condition serious-condition)
  ()
  (:documentation "Base class for serious VM conditions."))

(define-condition vm-simple-condition (vm-condition simple-condition)
  ()
  (:documentation "VM condition carrying FORMAT-CONTROL and FORMAT-ARGUMENTS."))

(define-condition vm-error (vm-serious-condition error)
  ()
  (:documentation "Base class for VM errors. These are serious conditions that
typically require intervention to continue execution."))

(define-condition vm-fatal-error (vm-error)
  ((message :initarg :message :reader vm-fatal-error-message)
   (print-backtrace-p :initarg :print-backtrace-p
                      :initform t
                      :reader vm-fatal-error-print-backtrace-p))
  (:report (lambda (condition stream)
             (princ (vm-fatal-error-message condition) stream)))
  (:documentation "VM error used for PHP fatal errors with optional backtraces."))

(define-condition vm-warning (vm-condition warning)
  ()
  (:documentation "Base class for VM warnings. These indicate potential issues
but don't interrupt normal execution."))

(define-condition vm-simple-error (vm-error simple-error)
  ()
  (:report (lambda (condition stream)
             (apply #'format-rich-condition-report stream
                    (simple-condition-format-control condition)
                    condition
                    (simple-condition-format-arguments condition))))
  (:documentation "Simple VM error with FORMAT-CONTROL/FORMAT-ARGUMENTS."))

(define-condition vm-simple-warning (vm-warning simple-warning)
  ()
  (:report (lambda (condition stream)
             (apply #'format-rich-condition-report stream
                    (simple-condition-format-control condition)
                    condition
                    (simple-condition-format-arguments condition))))
  (:documentation "Simple VM warning with FORMAT-CONTROL/FORMAT-ARGUMENTS."))

(define-condition vm-type-error (vm-error type-error)
  ()
  (:documentation "Type mismatch error - raised when a value doesn't match
the expected type.  Inherits from CL's TYPE-ERROR so user code can catch it
via (handler-case ... (type-error (c) ...)).")
  (:report (lambda (condition stream)
             (format stream "VM Type Error: expected ~A, got ~S"
                     (type-error-expected-type condition)
                     (type-error-datum condition)))))

(define-condition vm-unbound-variable (vm-error unbound-variable)
  ()
  (:documentation "Error raised when accessing an undefined variable.
Inherits from CL's UNBOUND-VARIABLE so user code can catch it via
(handler-case ... (unbound-variable (c) ...)).")
  (:report (lambda (condition stream)
             (format-rich-condition-report stream "VM Unbound Variable: ~S" condition
                                           (cell-error-name condition)))))

(define-condition vm-undefined-function (vm-error undefined-function)
  ()
  (:documentation "Error raised when calling an undefined function.
Inherits from CL's UNDEFINED-FUNCTION so user code can catch it via
(handler-case ... (undefined-function (c) ...)).")
  (:report (lambda (condition stream)
             (format-rich-condition-report stream "VM Undefined Function: ~S" condition
                                           (cell-error-name condition)))))

(define-condition vm-arithmetic-error (vm-error arithmetic-error) ()
  (:documentation "Error signaled when an arithmetic operation fails."))

(define-condition vm-division-by-zero (vm-arithmetic-error division-by-zero)
  ((dividend :initarg :dividend :reader vm-dividend))
  (:report (lambda (c s) (format s "VM Division By Zero: attempted to divide ~S by zero" (vm-dividend c)))))

(define-condition vm-floating-point-overflow (vm-arithmetic-error floating-point-overflow) ()
  (:documentation "Error signaled when a floating-point operation overflows."))

(define-condition vm-floating-point-underflow (vm-arithmetic-error floating-point-underflow) ()
  (:documentation "Error signaled when a floating-point operation underflows."))

(define-condition vm-cell-error (vm-error cell-error) ()
  (:documentation "Error signaled when accessing an unbound cell."))

(define-condition vm-unbound-slot (vm-cell-error unbound-slot) ()
  (:documentation "Error signaled when accessing an unbound slot."))

(define-condition vm-control-error (vm-error control-error) ()
  (:documentation "Error signaled for invalid dynamic control transfer."))

(define-condition vm-program-error (vm-control-error program-error) ()
  (:documentation "Error signaled for malformed programs or invalid syntax."))

(define-condition vm-stream-error (vm-error stream-error) ()
  (:documentation "Error signaled for stream-related errors."))

(define-condition vm-end-of-file (vm-stream-error end-of-file) ()
  (:documentation "Error signaled when reading past end of file."))

(define-condition vm-reader-error (vm-stream-error reader-error) ()
  (:documentation "Error signaled when the reader encounters invalid input."))

(define-condition vm-package-error (vm-error package-error) ()
  (:documentation "Error signaled for package-related errors."))

(define-condition vm-storage-condition (vm-condition storage-condition) ()
  (:documentation "Error signaled when storage is exhausted."))

(define-condition vm-style-warning (vm-warning style-warning) ()
  (:documentation "Warning about style issues."))

(defun %vm-condition-printer-name (condition)
  "Return the summary class name used for escaped condition printing."
  (cond ((typep condition 'error) "ERROR")
        ((typep condition 'warning) "WARNING")
        ((typep condition 'serious-condition) "SERIOUS-CONDITION")
        (t "CONDITION")))

(defun %vm-condition-report-string (condition)
  "Return CONDITION's human-readable report without escaped object syntax."
  (let ((*print-escape* nil))
    (princ-to-string condition)))

(defmethod print-object ((condition vm-condition) stream)
  "Print VM conditions as #<ERROR: report> when escaped, otherwise report them.

The unescaped branch delegates to the host condition reporter, preserving
DEFINE-CONDITION :REPORT behavior for VM condition subclasses."
  (if *print-escape*
      (print-unreadable-object (condition stream)
        (format stream "~A: ~A"
                (%vm-condition-printer-name condition)
                (%vm-condition-report-string condition)))
      (call-next-method)))
