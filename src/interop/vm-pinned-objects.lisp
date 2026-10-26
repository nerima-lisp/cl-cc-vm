(in-package #:cl-cc/vm/interop)

;; FR-856: Pinned Objects for FFI
(defmacro with-pinned-object ((obj) &body body)
  `(progn (register-pin ,obj) (unwind-protect (progn ,@body) (unregister-pin ,obj))))

(defun register-pin (obj) (declare (ignore obj)) t)

(defun unregister-pin (obj) (declare (ignore obj)) t)

;; FR-857: GC External Roots
(defun register-gc-root (ptr) (declare (ignore ptr)) t)
