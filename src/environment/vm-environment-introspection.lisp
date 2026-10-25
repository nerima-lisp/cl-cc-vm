(in-package :cl-cc/vm)

;;; ─── FR-612: Environment Introspection API ──────────────────────────────────
;;;
;;; The CL:LISP-IMPLEMENTATION-TYPE/MACHINE-TYPE/SOFTWARE-TYPE/ROOM/APROPOS
;;; family, shadowed in vm.lisp (loaded before this file) and redefined here
;;; to identify as CL-CC and pass most queries straight through to the host.

(defun lisp-implementation-type ()
  "CL-CC")

(defun lisp-implementation-version ()
  "0.1.0")

(defmacro define-host-environment-query (name &optional (default nil default-p))
  "Define NAME as a zero-argument passthrough to CL:NAME, the shadowed host
function. DEFAULT, when supplied, replaces a null host return value (SBCL
leaves the site-name accessors unconfigured by default)."
  (let ((host-name (intern (symbol-name name) :cl)))
    (if default-p
        `(defun ,name () (or (,host-name) ,default))
        `(defun ,name () (,host-name)))))

(define-host-environment-query long-site-name "unknown")

(define-host-environment-query short-site-name "unknown")

(define-host-environment-query software-version)

(define-host-environment-query software-type)

(define-host-environment-query machine-instance)

(define-host-environment-query machine-version)

(define-host-environment-query machine-type)

(defun room (&optional (stream cl:*standard-output*))
  (cl:format stream "; CL-CC ~A ~A on ~A ~A~%"
             (lisp-implementation-type) (lisp-implementation-version)
             (machine-type) (software-type)))

(defun apropos-list (string-designator &optional (package nil package-supplied-p))
  (let ((result nil)
        (string (cl:string string-designator))
        (packages (cl:if package-supplied-p
                         (cl:list (cl:find-package package))
                         (cl:list-all-packages))))
    (cl:dolist (pkg packages result)
      (cl:do-symbols (sym pkg)
        (cl:when (cl:search string (cl:symbol-name sym) :test #'cl:char-equal)
          (cl:pushnew sym result))))))

(defun apropos (string-designator &optional package)
  (cl:dolist (sym (apropos-list string-designator package))
    (cl:format cl:t "~A~%" sym))
  (cl:values))
