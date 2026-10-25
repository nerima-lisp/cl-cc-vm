;;;; t/vm-package-designator-test.lisp — VM package-designator instructions
;;;;
;;;; Exercises package operations end to end through RUN-COMPILED with runtime
;;;; package descriptors and CLHS string-designator forms.

(in-package :cl-cc-vm/test)

(progn
  (defun %vm-package-designator-fresh-name ()
    (format nil "VM-PACKAGE-DESIGNATOR-~A" (symbol-name (gensym))))

  (defun %vm-package-designator-fresh-character ()
    (loop for code from 33 to 126
          for character = (code-char code)
          for name = (string character)
          unless (or (cl-cc/runtime:rt-find-package name)
                     (find-package name))
            return character
          finally (error "No unused package-name character is available")))

  (defun %vm-package-designator-new-package (&optional name)
    (cl-cc/runtime:rt-make-package
     (or name (%vm-package-designator-fresh-name))
     :use nil))

  (defun %vm-package-designator-cases ()
    (let* ((name (%vm-package-designator-fresh-name))
           (package (%vm-package-designator-new-package name))
           (character (%vm-package-designator-fresh-character))
           (character-package
             (%vm-package-designator-new-package (string character)))
           (immediate-symbol
             (loop for symbol across cl-cc/vm::*vm-immediate-symbol-table*
                   for package-name = (symbol-name symbol)
                   when (and (eq (symbol-package symbol) (find-package :cl))
                             (not (cl-cc/runtime:rt-find-package package-name))
                             (not (find-package package-name)))
                     return symbol
                   finally (error "No unused common immediate symbol is available")))
           (immediate-package
             (%vm-package-designator-new-package
              (symbol-name immediate-symbol))))
      (list (list :string name package)
            (list :symbol (make-symbol name) package)
            (list :character character character-package)
            (list :descriptor package package)
            (list :immediate-symbol
                  (cl-cc/vm::vm-encode-common-symbol immediate-symbol)
                  immediate-package))))

  (defun %vm-package-designator-unknowns ()
    (let ((name (%vm-package-designator-fresh-name)))
      (list name
            (make-symbol name)
            (%vm-package-designator-fresh-character))))

  (defun %vm-package-designator-run (bindings instruction)
    (let ((program
            (cl-cc/vm:make-vm-program
             :instructions
             (append
              (loop for (register value) in bindings
                    collect (cl-cc/vm:make-vm-const :dst register :value value))
              (list instruction
                    (cl-cc/vm:make-vm-halt :reg :result)))
             :result-register :result)))
      (cl-cc/vm:run-compiled program)))

  (defun %vm-package-designator-signals-error-p (thunk)
    (handler-case
        (progn (funcall thunk) nil)
      (package-error () t)))

  (describe-sequential "VM package designators"

    (it "resolves string, symbol, character, immediate symbol, and descriptor names"
      (let ((cases (%vm-package-designator-cases)))
        (expect (cl-cc/vm::vm-immediate-symbol-p (second (fifth cases))) :to-be t)
        (dolist (case cases)
          (destructuring-bind (kind designator package) case
            (declare (ignore kind))
            (expect
             (eq (%vm-package-designator-run
                  (list (list :r0 designator))
                  (cl-cc/vm:make-vm-find-package :dst :result :src :r0))
                 package)
             :to-be t)
            (expect
             (string=
              (cl-cc/vm::vm-string-materialize
               (%vm-package-designator-run
                (list (list :r0 designator))
                (cl-cc/vm:make-vm-package-name-inst :dst :result :pkg :r0)))
              (cl-cc/runtime:rt-package-name package))
             :to-be t)))
        (dolist (designator (%vm-package-designator-unknowns))
          (expect
           (null (%vm-package-designator-run
                  (list (list :r0 designator))
                  (cl-cc/vm:make-vm-find-package :dst :result :src :r0)))
           :to-be t)
          (expect
           (%vm-package-designator-signals-error-p
            (lambda ()
              (%vm-package-designator-run
               (list (list :r0 designator))
               (cl-cc/vm:make-vm-package-name-inst :dst :result :pkg :r0))))
           :to-be t))))

    (it "uses all name forms for INTERN and FIND-SYMBOL"
      (let ((cases (%vm-package-designator-cases))
            (symbol-name (%vm-package-designator-fresh-name)))
        (dolist (case cases)
          (destructuring-bind (kind designator package) case
            (declare (ignore kind))
            (let ((symbol
                    (%vm-package-designator-run
                     (list (list :r0 symbol-name)
                           (list :r1 designator))
                     (cl-cc/vm:make-vm-intern-symbol
                      :dst :result :src :r0 :pkg :r1))))
              (expect
               (eq (%vm-package-designator-run
                    (list (list :r0 symbol-name)
                          (list :r1 designator))
                    (cl-cc/vm:make-vm-find-symbol
                     :dst :result :src :r0 :pkg :r1))
                   symbol)
               :to-be t)
              (multiple-value-bind (found status)
                  (cl-cc/runtime::rt-find-symbol symbol-name package)
                (expect (eq found symbol) :to-be t)
                (expect (eq status :internal) :to-be t)))))
        (dolist (designator (%vm-package-designator-unknowns))
          (expect
           (%vm-package-designator-signals-error-p
            (lambda ()
              (%vm-package-designator-run
               (list (list :r0 symbol-name)
                     (list :r1 designator))
               (cl-cc/vm:make-vm-intern-symbol
                :dst :result :src :r0 :pkg :r1))))
           :to-be t)
          (expect
           (%vm-package-designator-signals-error-p
            (lambda ()
              (%vm-package-designator-run
               (list (list :r0 symbol-name)
                     (list :r1 designator))
               (cl-cc/vm:make-vm-find-symbol
                :dst :result :src :r0 :pkg :r1))))
           :to-be t))))

    (it "resolves package names for IMPORT and EXPORT"
      (let ((cases (%vm-package-designator-cases))
            (unknown-symbol-name (%vm-package-designator-fresh-name)))
        (dolist (case cases)
          (destructuring-bind (kind designator package) case
            (declare (ignore kind))
            (let* ((symbol-name (%vm-package-designator-fresh-name))
                   (source (%vm-package-designator-new-package))
                   (symbol (cl-cc/runtime:rt-intern symbol-name source)))
              (cl-cc/runtime:rt-export (list symbol) source)
              (%vm-package-designator-run
               (list (list :r0 (list symbol))
                     (list :r1 designator))
               (cl-cc/vm:make-vm-import-inst
                :dst :result :symbols :r0 :pkg :r1))
              (multiple-value-bind (found status)
                  (cl-cc/runtime::rt-find-symbol symbol-name package)
                (expect (eq found symbol) :to-be t)
                (expect (eq status :internal) :to-be t))
              (%vm-package-designator-run
               (list (list :r0 (list symbol))
                     (list :r1 designator))
               (cl-cc/vm:make-vm-export-inst
                :dst :result :symbols :r0 :pkg :r1))
              (multiple-value-bind (found status)
                  (cl-cc/runtime::rt-find-symbol symbol-name package)
                (expect (eq found symbol) :to-be t)
                (expect (eq status :external) :to-be t)))))
        (dolist (designator (%vm-package-designator-unknowns))
          (expect
           (%vm-package-designator-signals-error-p
            (lambda ()
              (%vm-package-designator-run
               (list (list :r0 (list (make-symbol unknown-symbol-name)))
                     (list :r1 designator))
               (cl-cc/vm:make-vm-import-inst
                :dst :result :symbols :r0 :pkg :r1))))
           :to-be t)
          (expect
           (%vm-package-designator-signals-error-p
            (lambda ()
              (%vm-package-designator-run
               (list (list :r0 (list (make-symbol unknown-symbol-name)))
                     (list :r1 designator))
               (cl-cc/vm:make-vm-export-inst
                :dst :result :symbols :r0 :pkg :r1))))
           :to-be t))))

    (it "resolves package names for SHADOW and UNINTERN"
      (let ((cases (%vm-package-designator-cases))
            (symbol-name (%vm-package-designator-fresh-name)))
        (dolist (case cases)
          (destructuring-bind (kind designator package) case
            (declare (ignore kind))
            (%vm-package-designator-run
             (list (list :r0 (list symbol-name))
                   (list :r1 designator))
             (cl-cc/vm:make-vm-shadow-inst
              :dst :result :symbol-names :r0 :pkg :r1))
            (multiple-value-bind (symbol status)
                (cl-cc/runtime::rt-find-symbol symbol-name package)
              (expect (not (null symbol)) :to-be t)
              (expect (eq status :internal) :to-be t)
              (%vm-package-designator-run
               (list (list :r0 symbol)
                     (list :r1 designator))
               (cl-cc/vm:make-vm-unintern-inst
                :dst :result :symbol :r0 :pkg :r1))
              (multiple-value-bind (found ignored-status)
                  (cl-cc/runtime::rt-find-symbol symbol-name package)
                (declare (ignore ignored-status))
                (expect (null found) :to-be t)))))
        (dolist (designator (%vm-package-designator-unknowns))
          (expect
           (%vm-package-designator-signals-error-p
            (lambda ()
              (%vm-package-designator-run
               (list (list :r0 (list symbol-name))
                     (list :r1 designator))
               (cl-cc/vm:make-vm-shadow-inst
                :dst :result :symbol-names :r0 :pkg :r1))))
           :to-be t)
          (expect
           (%vm-package-designator-signals-error-p
            (lambda ()
              (%vm-package-designator-run
               (list (list :r0 (make-symbol symbol-name))
                     (list :r1 designator))
               (cl-cc/vm:make-vm-unintern-inst
                :dst :result :symbol :r0 :pkg :r1))))
           :to-be t))))

    (it "resolves both USE-PACKAGE and UNUSE-PACKAGE designators"
      (dolist (target-case (%vm-package-designator-cases))
        (destructuring-bind (target-kind target-designator target-package)
            target-case
          (declare (ignore target-kind target-package))
          (dolist (source-case (%vm-package-designator-cases))
            (destructuring-bind (source-kind source-designator source-package)
                source-case
              (declare (ignore source-kind))
              (let* ((symbol-name (%vm-package-designator-fresh-name))
                     (symbol
                       (cl-cc/runtime:rt-intern symbol-name source-package)))
                (cl-cc/runtime:rt-export (list symbol) source-package)
                (%vm-package-designator-run
                 (list (list :r0 (list source-designator))
                       (list :r1 target-designator))
                 (cl-cc/vm:make-vm-use-package-inst
                  :dst :result :packages-to-use :r0 :pkg :r1))
                (multiple-value-bind (found status)
                    (cl-cc/runtime::rt-find-symbol symbol-name
                                                   (third target-case))
                  (expect (eq found symbol) :to-be t)
                  (expect (eq status :inherited) :to-be t))
                (%vm-package-designator-run
                 (list (list :r0 (list source-designator))
                       (list :r1 target-designator))
                 (cl-cc/vm:make-vm-unuse-package-inst
                  :dst :result :packages-to-unuse :r0 :pkg :r1))
                (multiple-value-bind (found ignored-status)
                    (cl-cc/runtime::rt-find-symbol symbol-name
                                                   (third target-case))
                  (declare (ignore ignored-status))
                  (expect (null found) :to-be t)))))))
      (let* ((cases (%vm-package-designator-cases))
             (known-designator (second (third cases)))
             (target-package (third (first cases))))
        (dolist (unknown (%vm-package-designator-unknowns))
          (expect
           (%vm-package-designator-signals-error-p
            (lambda ()
              (%vm-package-designator-run
               (list (list :r0 (list unknown))
                     (list :r1 target-package))
               (cl-cc/vm:make-vm-use-package-inst
                :dst :result :packages-to-use :r0 :pkg :r1))))
           :to-be t)
          (expect
           (%vm-package-designator-signals-error-p
            (lambda ()
              (%vm-package-designator-run
               (list (list :r0 (list unknown))
                     (list :r1 target-package))
               (cl-cc/vm:make-vm-unuse-package-inst
                :dst :result :packages-to-unuse :r0 :pkg :r1))))
           :to-be t)
          (expect
           (%vm-package-designator-signals-error-p
            (lambda ()
              (%vm-package-designator-run
               (list (list :r0 (list known-designator))
                     (list :r1 unknown))
               (cl-cc/vm:make-vm-use-package-inst
                :dst :result :packages-to-use :r0 :pkg :r1))))
           :to-be t)
          (expect
           (%vm-package-designator-signals-error-p
            (lambda ()
              (%vm-package-designator-run
               (list (list :r0 (list known-designator))
                     (list :r1 unknown))
               (cl-cc/vm:make-vm-unuse-package-inst
                :dst :result :packages-to-unuse :r0 :pkg :r1))))
           :to-be t))))

    (it "maps host CL to its runtime mirror and preserves CL:CAR identity"
      (let* ((host-package (find-package :cl))
             (runtime-package
               (or (cl-cc/runtime:rt-find-package "CL")
                   (cl-cc/runtime:rt-find-package "COMMON-LISP")))
             (host-symbol (find-symbol "CAR" host-package))
             (resolved-package
               (%vm-package-designator-run
                (list (list :r0 host-package))
                (cl-cc/vm:make-vm-find-package :dst :result :src :r0)))
             (symbol-through-host-package
               (%vm-package-designator-run
                (list (list :r0 "CAR")
                      (list :r1 host-package))
                (cl-cc/vm:make-vm-find-symbol
                 :dst :result :src :r0 :pkg :r1)))
             (symbol-through-runtime-package
               (%vm-package-designator-run
                (list (list :r0 "CAR")
                      (list :r1 runtime-package))
                (cl-cc/vm:make-vm-find-symbol
                 :dst :result :src :r0 :pkg :r1)))
             (interned-through-host-package
               (%vm-package-designator-run
                (list (list :r0 "CAR")
                      (list :r1 host-package))
                (cl-cc/vm:make-vm-intern-symbol
                 :dst :result :src :r0 :pkg :r1))))
        (expect (not (null runtime-package)) :to-be t)
        (expect (eq (gethash :host-package runtime-package) host-package)
                :to-be t)
        (expect (eq resolved-package runtime-package) :to-be t)
        (expect (eq symbol-through-host-package host-symbol) :to-be t)
        (expect (eq symbol-through-runtime-package host-symbol) :to-be t)
        (expect (and (cl-cc/vm::vm-immediate-symbol-p interned-through-host-package)
                     (eq (cl-cc/vm::vm-decode-symbol interned-through-host-package)
                         host-symbol))
                :to-be t)))))
