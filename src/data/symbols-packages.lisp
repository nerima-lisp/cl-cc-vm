(in-package #:cl-cc/vm/data)

;;; Host package/symbol operations (bootstrap-package-registry, intern,
;;; export/import, use-package, shadow, rename/delete-package, and the
;;; package-local-nickname helpers) that back the VM instruction macros and
;;; their execute-instruction methods in symbols.lisp (loads next).

(defun %vm-resolve-package-designator (designator)
  (labels ((registered-runtime-package-p (candidate)
             (let ((registry cl-cc/runtime::*rt-package-registry*))
               (and (hash-table-p registry)
                    (hash-table-p candidate)
                    (loop for package being the hash-values of registry
                          thereis (eq package candidate)))))
           (runtime-package-for-host-package (host-package)
             (let ((registry cl-cc/runtime::*rt-package-registry*))
               (when (and (hash-table-p registry) (packagep host-package))
                 (loop for package being the hash-values of registry
                       when (and (hash-table-p package)
                                 (eq host-package (gethash :host-package package)))
                         return package))))
           (runtime-package-host-designator (package)
             (let ((host-package (gethash :host-package package))
                   (name (gethash :name package)))
               (cond ((packagep host-package) host-package)
                     ((or (stringp name) (symbolp name) (characterp name)) name)
                     (t package))))
           (string-designator-name (value)
             (let ((value (if (vm-immediate-symbol-p value)
                              (vm-decode-symbol value)
                              (%vm-host-string value))))
               (typecase value
                 (string value)
                 (symbol (symbol-name value))
                 (character (string value))
                 (t nil))))
           (resolve-name (name)
             (let ((nickname (%vm-find-package-local-nickname name)))
               (if nickname
                   (values (runtime-package-for-host-package nickname) nickname)
                   (let ((candidate
                           (and (hash-table-p cl-cc/runtime::*rt-package-registry*)
                                (cl-cc/runtime:rt-find-package name))))
                     (if (registered-runtime-package-p candidate)
                         (values candidate
                                 (runtime-package-host-designator candidate))
                         (values nil name)))))))
    (let ((name (string-designator-name designator)))
      (cond
        ((registered-runtime-package-p designator)
         (values designator (runtime-package-host-designator designator)))
        ((packagep designator)
         (values (runtime-package-for-host-package designator) designator))
        (name
         (resolve-name name))
        (t
         (values nil designator))))))

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
  (multiple-value-bind (runtime-package)
      (%vm-resolve-package-designator designator)
    (or runtime-package
        (and errorp
             (error "Package ~S not found in runtime registry" designator)))))

(defun vm-intern-symbol (name &optional package)
  "Intern NAME in PACKAGE using the runtime package registry."
  (cl-cc/runtime:rt-intern
   (string (%vm-host-string name))
   (vm-find-package (or package "CL-USER"))))

(defun vm-find-symbol (name &optional package) "Find NAME in PACKAGE using the runtime package registry." (let* ((key (string (%vm-host-string name))) (designator (or package "CL-USER"))) (multiple-value-bind (runtime-package host-designator) (%vm-resolve-package-designator designator) (if runtime-package (let ((host-package (gethash :host-package runtime-package))) (multiple-value-bind (symbol status) (cl-cc/runtime::rt-find-symbol key runtime-package) (multiple-value-bind (host-symbol host-status) (if host-package (find-symbol key host-package) (values nil nil)) (cond ((and status host-status (eq host-symbol symbol)) (values symbol host-status)) ((or symbol status) (values symbol status)) (host-status (values host-symbol host-status)) (t (values nil status)))))) (find-symbol key host-designator)))))

(defun vm-export (symbols &optional package)
  "Export SYMBOLS from PACKAGE in the runtime package registry."
  (cl-cc/runtime:rt-export
   symbols
   (vm-find-package (or package "CL-USER"))))

(defun vm-import (symbols &optional package)
  "Import SYMBOLS into PACKAGE in the runtime package registry."
  (cl-cc/runtime::rt-import
   symbols
   (vm-find-package (or package "CL-USER"))))

(defun vm-use-package (packages-to-use &optional package)
  "Use PACKAGES-TO-USE from PACKAGE in the runtime package registry."
  (cl-cc/runtime::rt-use-package
   (if (listp packages-to-use)
       (mapcar (lambda (designator) (vm-find-package designator))
               packages-to-use)
       (vm-find-package packages-to-use))
   (vm-find-package (or package "CL-USER"))))

(defun vm-unuse-package (packages-to-unuse &optional package)
  "Unuse PACKAGES-TO-UNUSE from PACKAGE in the runtime package registry."
  (cl-cc/runtime::rt-unuse-package
   (if (listp packages-to-unuse)
       (mapcar (lambda (designator) (vm-find-package designator))
               packages-to-unuse)
       (vm-find-package packages-to-unuse))
   (vm-find-package (or package "CL-USER"))))

(defun vm-shadow (symbol-names &optional package)
  "Shadow SYMBOL-NAMES in PACKAGE using the runtime package registry."
  (cl-cc/runtime::rt-shadow
   symbol-names
   (vm-find-package (or package "CL-USER"))))

(defun vm-unintern (symbol &optional package)
  "Unintern SYMBOL from PACKAGE using the runtime package registry."
  (cl-cc/runtime::rt-unintern
   symbol
   (vm-find-package (or package "CL-USER"))))

(defun vm-package-name (package)
  "Return the runtime name of PACKAGE."
  (cl-cc/runtime:rt-package-name (vm-find-package package)))

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

(defun %vm-package-operation-result (pkg runtime-fn host-fn args &optional package-list-index)
  (labels ((resolve-member (designator runtime-p)
             (multiple-value-bind (runtime-package host-designator)
                 (%vm-resolve-package-designator designator)
               (if runtime-p
                   (or runtime-package
                       (error 'package-error :package designator))
                   host-designator)))
           (map-members (members runtime-p)
             (if (listp members)
                 (mapcar (lambda (designator)
                           (resolve-member designator runtime-p))
                         members)
                 (resolve-member members runtime-p))))
    (multiple-value-bind (runtime-package host-designator)
        (%vm-resolve-package-designator pkg)
      (if runtime-package
          (let ((runtime-args (copy-list args)))
            (when package-list-index
              (setf (nth package-list-index runtime-args)
                    (map-members (nth package-list-index args) t)))
            (setf (car (last runtime-args)) runtime-package)
            (apply runtime-fn runtime-args))
          (let ((host-args (copy-list args)))
            (when package-list-index
              (setf (nth package-list-index host-args)
                    (map-members (nth package-list-index args) nil)))
            (setf (car (last host-args)) host-designator)
            (apply host-fn host-args))))))

(defun %vm-package-result (pkg runtime-fn host-fn &rest args)
  (multiple-value-bind (runtime-package host-designator)
      (%vm-resolve-package-designator pkg)
    (let ((resolved-args
            (append (butlast args)
                    (list (if runtime-package runtime-package host-designator)))))
      (if runtime-package
          (apply runtime-fn resolved-args)
          (apply host-fn resolved-args)))))

(defun %vm-registry-result (runtime-fn host-fn &rest args)
  (if cl-cc/runtime::*rt-package-registry*
      (apply runtime-fn args)
      (apply host-fn args)))
