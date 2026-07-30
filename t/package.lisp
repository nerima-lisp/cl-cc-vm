;;;; t/package.lisp — test package for cl-cc-vm
(defpackage :cl-cc-vm/test (:use :cl :cl-weave)
  (:shadowing-import-from :cl-weave :describe))
