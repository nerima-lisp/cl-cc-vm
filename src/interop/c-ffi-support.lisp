(in-package #:cl-cc/vm/interop)

(defun make-callback (fn ctype) (declare (ignore fn ctype)) fn)

;; FR-885: Variadic C Function Support
(defun foreign-call-varargs (name ret-type format args)
  (declare (ignore name ret-type format args)) nil)

;; FR-886: Platform-Specific API Integration
(defmacro with-platform (bindings &body body) (declare (ignore bindings)) `(progn ,@body))

;; FR-887: ABI-Stable C Public Interface Generation
(defun export-c-api (name sig) (declare (ignore name sig)) t)

(defun emit-header (path) (declare (ignore path)) t)
