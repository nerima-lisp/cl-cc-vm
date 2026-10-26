(in-package #:cl-cc/vm/environment)

(defvar *feature-flags* (make-hash-table :test #'eq))

(defmacro when-feature (flag &body body) `(when (gethash ,flag *feature-flags*) ,@body))

(defvar *liquid-types-enabled* nil)
