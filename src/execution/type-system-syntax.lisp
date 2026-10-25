(in-package #:cl-cc/vm/execution)

(defmacro deftype-rec (name &rest body) (declare (ignore body)) `(deftype ,name () 't))

;; FR-867: Codata / Coinductive Types
(defmacro defcodata (name &rest fields) (declare (ignore fields)) `(defstruct ,name))

;; FR-869: Opaque Types / Abstract Type Boundaries
(defmacro defopaque-type (name underlying) (declare (ignore underlying)) `(deftype ,name () 't))
