;;; vm-serialize.lisp — FR-1042: Object serialization
;;;
;;; make-load-form, FASL serialization, and print-unreadable-object.

(in-package :cl-cc/vm)

(defun vm-make-load-form (object &optional environment)
  (handler-case
      (make-load-form object environment)
    (error () `',object)))

(defun vm-make-load-form-saving-slots (object &key slot-names environment)
  (declare (ignore environment))
  (let ((slots (or slot-names (%vm-serialize-slot-names object))))
    `(make-instance ',(class-name (class-of object))
                    ,@(loop for name in slots
                            when (slot-boundp object name)
                              append (list (intern (symbol-name name) :keyword)
                                           `',(slot-value object name))))))

(defun %vm-serialize-slot-names (object)
  (handler-case
      (loop for slot in (class-slots (class-of object))
            for name = (slot-definition-name slot)
            when (slot-boundp object name) collect name)
    (error () nil)))

(defvar *fasl-code-version* 1)
(defvar +vm-fasl-magic+ "CLCC-FASL1")

(progn
  (defun %vm-load-time-value-cell->wire (cell)
    (if (vm-load-time-value-cell-resolved-p cell)
        (list :vm-fasl-load-time-value-cell
              (vm-load-time-value-cell-id cell)
              :resolved
              (vm-load-time-value-cell-value cell))
        (list :vm-fasl-load-time-value-cell
              (vm-load-time-value-cell-id cell)
              :unresolved
              (vm-load-time-value-cell-form cell)
              (vm-load-time-value-cell-read-only-p cell))))

  (defun %vm-wire->load-time-value-cell (wire)
    (destructuring-bind (tag id state &rest payload) wire
      (unless (eq tag :vm-fasl-load-time-value-cell)
        (error "Invalid load-time-value cell wire value: ~S" wire))
      (ecase state
        (:resolved
         (make-vm-load-time-value-cell
          :id id :resolved-p t :value (first payload)))
        (:unresolved
         (make-vm-load-time-value-cell
          :id id :form (first payload) :read-only-p (second payload))))))

  (defun %vm-program->wire (program)
    (list :vm-fasl-program
          :instructions (mapcar (function instruction->sexp)
                                (vm-program-instructions program))
          :result-register (vm-program-result-register program)
          :leaf-p (vm-program-leaf-p program)
          :calling-convention (vm-program-calling-convention program)
          :function-conventions (vm-program-function-conventions program)
          :deopt-info (vm-program-deopt-info program)
          :osr-entry-points (vm-program-osr-entry-points program)
          :tier1-entry-points (vm-program-tier1-entry-points program)
          :load-time-value-cells
          (mapcar (function %vm-load-time-value-cell->wire)
                  (vm-program-load-time-value-cells program))
          :compilation-tier (vm-program-compilation-tier program)))

  (defun %vm-wire->program (wire)
    (let ((fields (cdr wire)))
      (make-vm-program
       :instructions (mapcar (function sexp->instruction)
                             (getf fields :instructions))
       :result-register (getf fields :result-register)
       :leaf-p (getf fields :leaf-p)
       :calling-convention (getf fields :calling-convention)
       :function-conventions (getf fields :function-conventions)
       :deopt-info (getf fields :deopt-info)
       :osr-entry-points (getf fields :osr-entry-points)
       :tier1-entry-points (getf fields :tier1-entry-points)
       :load-time-value-cells
       (mapcar (function %vm-wire->load-time-value-cell)
               (getf fields :load-time-value-cells))
       :compilation-tier (getf fields :compilation-tier))))

  (defun %vm-value->wire (value)
    (typecase value
      (vm-program (%vm-program->wire value))
      (vm-load-time-value-cell (%vm-load-time-value-cell->wire value))
      (t (list :vm-fasl-value value))))

  (defun %vm-wire->value (wire)
    (case (and (consp wire) (car wire))
      (:vm-fasl-program (%vm-wire->program wire))
      (:vm-fasl-load-time-value-cell (%vm-wire->load-time-value-cell wire))
      (:vm-fasl-value (second wire))
      (t (error "Invalid CL-CC FASL payload: ~S" wire))))

  (defun vm-write-to-fasl (value stream)
    (write-string +vm-fasl-magic+ stream)
    (terpri stream)
    (let ((*print-circle* t)
          (*print-readably* t))
      (write (%vm-value->wire value) :stream stream :circle t :readably t))
    value))

(defun vm-read-from-fasl (stream)
  (let ((magic (read-line stream nil nil)))
    (unless (string= magic +vm-fasl-magic+)
      (error "Invalid CL-CC FASL magic: ~S" magic))
    (let ((*read-eval* nil))
      (%vm-wire->value (read stream t nil)))))

(defun vm-compile-file-to-fasl (source-path &key output-file)
  (compile-file source-path :output-file (or output-file
        (make-pathname :type "fasl" :defaults source-path))))

(defun vm-load-fasl (fasl-path) (load fasl-path))

(defmacro vm-print-unreadable-object ((object stream &key type identity) &body body)
  `(progn
     (write-string "#<" ,stream)
     (when ,type
       (write-string (symbol-name (class-name (class-of ,object))) ,stream)
       (write-char #\Space ,stream))
     ,@body
     (when ,identity
         (format ,stream " {~X}" (sxhash ,object)))
     (write-char #\> ,stream)))

(export '(vm-make-load-form vm-make-load-form-saving-slots
          vm-write-to-fasl vm-read-from-fasl vm-compile-file-to-fasl
          vm-load-fasl vm-print-unreadable-object *fasl-code-version*))
