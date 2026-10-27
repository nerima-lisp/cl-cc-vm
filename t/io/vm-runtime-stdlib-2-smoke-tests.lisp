(in-package #:cl-cc/vm/io/test)

(defun register-vm-runtime-stdlib-2-smoke-tests ()
  (it-sequential "fr-function-exists fr-851-socket"
                   (destructuring-bind (sym) (list 'cl-cc/vm::make-tcp-socket)
                     (expect (fboundp sym) :to-be-truthy)))

  (it-sequential "fr-function-exists fr-852-dns"
                   (destructuring-bind (sym) (list 'cl-cc/vm::dns-resolve)
                     (expect (fboundp sym) :to-be-truthy)))

  (it-sequential "fr-function-exists fr-853-tls"
                   (destructuring-bind (sym) (list 'cl-cc/vm::make-tls-context)
                     (expect (fboundp sym) :to-be-truthy)))

  (it-sequential "fr-function-exists fr-868-file-position"
                   (destructuring-bind (sym) (list 'cl-cc/vm::vm-file-position)
                     (expect (fboundp sym) :to-be-truthy)))

  (it-sequential "fr-function-exists fr-869-mmap"
                   (destructuring-bind (sym) (list 'cl-cc/runtime::mmap-file)
                     (expect (fboundp sym) :to-be-truthy)))

  (it-sequential "fr-function-exists fr-923-io-buffering"
                   (destructuring-bind (sym) (list 'cl-cc/vm::make-buffered-stream)
                     (expect (fboundp sym) :to-be-truthy)))

  (it-sequential "fr-function-exists fr-924-special-streams"
                   (destructuring-bind (sym) (list 'cl-cc/vm::make-string-input-stream)
                     (expect (fboundp sym) :to-be-truthy)))

  (it-sequential "fr-function-exists fr-927-pathname"
                   (destructuring-bind (sym) (list 'cl-cc/vm::wild-pathname-p)
                     (expect (fboundp sym) :to-be-truthy)))

  (it-sequential "fr-variable-exists fr-820-print-circle"
                   (destructuring-bind (sym) (list 'cl-cc/vm::*print-circle*)
                     (expect (boundp sym) :to-be-truthy)))
)
