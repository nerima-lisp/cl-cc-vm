(in-package :cl-cc/vm)

;;; Host package/symbol operations (bootstrap-package-registry, intern,
;;; export/import, use-package, shadow, rename/delete-package, and the
;;; package-local-nickname helpers) that back the VM instruction macros and
;;; their execute-instruction methods in symbols.lisp (loads next).

(defun %vm-host-package-designator (designator)
  (if (hash-table-p designator)
      (or (gethash :host-package designator)
          (gethash :name designator))
      designator))

;;; ─── Registry-backed package helpers ───────────────────────────────────────
;;;
;;; The VM instruction classes above are intentionally named after the VM
;;; operation they implement.  Common Lisp keeps class names and function names
;;; in separate namespaces, so these functions provide a direct, testable API for
;;; the same self-hosting package registry used by the bytecode instructions.

(defun vm-symbol-name (symbol-or-string)
  "Return SYMBOL-OR-STRING's symbol name as a host string."
  (if (stringp symbol-or-string)
      symbol-or-string
      (cl-cc/runtime:rt-symbol-name symbol-or-string)))

(defun vm-make-symbol (name)
  "Create an uninterned symbol named NAME."
  (cl-cc/runtime::rt-make-symbol (string name)))

(defun vm-gensym-inst (&optional (prefix "G") counter)
  "Generate a unique uninterned symbol for self-host package tests."
  (declare (ignore counter))
  (cl-cc/runtime:rt-gensym prefix))

(defun vm-find-package (designator &optional (errorp t))
  "Find DESIGNATOR in the runtime package registry."
  (or (cl-cc/runtime:rt-find-package designator)
      (and errorp (error "Package ~S not found in runtime registry" designator))))

(defun vm-intern-symbol (name &optional package)
  "Intern NAME in PACKAGE using the runtime package registry."
  (cl-cc/runtime:rt-intern (string name) (or package (vm-find-package "CL-USER"))))

(defun vm-find-symbol (name &optional package)
  "Find NAME in PACKAGE using the runtime package registry."
  (cl-cc/runtime::rt-find-symbol (string name) (or package (vm-find-package "CL-USER"))))

(defun vm-export (symbols &optional package)
  "Export SYMBOLS from PACKAGE in the runtime package registry."
  (cl-cc/runtime:rt-export symbols (or package (vm-find-package "CL-USER"))))

(defun vm-import (symbols &optional package)
  "Import SYMBOLS into PACKAGE in the runtime package registry."
  (cl-cc/runtime::rt-import symbols (or package (vm-find-package "CL-USER"))))

(defun vm-use-package (packages-to-use &optional package)
  "Use PACKAGES-TO-USE from PACKAGE in the runtime package registry."
  (cl-cc/runtime::rt-use-package packages-to-use (or package (vm-find-package "CL-USER"))))

(defun vm-unuse-package (packages-to-unuse &optional package)
  "Unuse PACKAGES-TO-UNUSE from PACKAGE in the runtime package registry."
  (cl-cc/runtime::rt-unuse-package packages-to-unuse (or package (vm-find-package "CL-USER"))))

(defun vm-shadow (symbol-names &optional package)
  "Shadow SYMBOL-NAMES in PACKAGE in the runtime package registry."
  (cl-cc/runtime::rt-shadow symbol-names (or package (vm-find-package "CL-USER"))))

(defun vm-unintern (symbol &optional package)
  "Unintern SYMBOL from PACKAGE in the runtime package registry."
  (cl-cc/runtime::rt-unintern symbol (or package (vm-find-package "CL-USER"))))

(defun vm-package-name (package)
  "Return PACKAGE's runtime name string."
  (cl-cc/runtime:rt-package-name package))

(defun vm-list-all-packages ()
  "Return all runtime package descriptors."
  (cl-cc/runtime::rt-list-all-packages))

(defun %vm-host-package-local-nickname-function (name)
  (or (find-symbol name :cl)
      (let ((pkg (find-package :sb-ext)))
        (and pkg (find-symbol name pkg)))))

(defun %vm-find-package-local-nickname (name &optional (package *package*))
  (let ((fn (%vm-host-package-local-nickname-function "PACKAGE-LOCAL-NICKNAMES")))
    (when (and fn package)
      (let ((entry (assoc (string name) (funcall fn package) :test #'string=)))
        (when entry (cdr entry))))))

(defun %vm-package-operation-result (pkg runtime-fn host-fn &rest args)
  (if (and cl-cc/runtime::*rt-package-registry* (hash-table-p pkg))
      (apply runtime-fn args)
      (apply host-fn args)))

(defun %vm-package-result (pkg runtime-fn host-fn &rest args)
  (if (hash-table-p pkg)
      (apply runtime-fn args)
      (apply host-fn args)))

(defun %vm-registry-result (runtime-fn host-fn &rest args)
  (if cl-cc/runtime::*rt-package-registry*
      (apply runtime-fn args)
      (apply host-fn args)))
