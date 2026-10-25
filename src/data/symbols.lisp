(in-package :cl-cc/vm)

;;; VM Symbol and Character Instructions
;;;
;;; The host package/symbol operations these instructions dispatch through
;;; are in symbols-packages.lisp (loads before this file).
;;;
;;; This file provides VM instructions for symbol manipulation
;;; (symbol-name, make-symbol, intern, gensym, keywordp) and
;;; character predicates (digit-char-p, alpha-char-p, upper/lower-case-p,
;;; char-upcase/downcase, stringp, characterp, parse-integer, alphanumericp).
;;;

;;; ─── Symbol Manipulation Instructions ───────────────────────────────────────

(define-vm-instruction vm-symbol-name (vm-instruction)
  "Get the name string of a symbol."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :symbol-name)
  (:sexp-slots dst src))

(define-vm-instruction vm-make-symbol (vm-instruction)
  "Create an uninterned symbol from a string."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :make-symbol)
  (:sexp-slots dst src))

(define-vm-instruction vm-intern-symbol (vm-instruction)
  "Intern a string as a symbol. Optional package designator."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (pkg nil :reader vm-intern-pkg)
  (:sexp-tag :intern)
  (:sexp-slots dst src pkg))

(define-vm-instruction vm-add-package-local-nickname (vm-instruction)
  "Add a package-local nickname."
  (dst nil :reader vm-dst)
  (pkg nil :reader vm-local-nickname-pkg)
  (nick nil :reader vm-local-nickname-nick)
  (target nil :reader vm-local-nickname-target)
  (:sexp-tag :add-package-local-nickname)
  (:sexp-slots dst pkg nick target))

(define-vm-instruction vm-remove-package-local-nickname (vm-instruction)
  "Remove a package-local nickname."
  (dst nil :reader vm-dst)
  (pkg nil :reader vm-local-nickname-pkg)
  (nick nil :reader vm-local-nickname-nick)
  (target nil :reader vm-local-nickname-target)
  (:sexp-tag :remove-package-local-nickname)
  (:sexp-slots dst pkg nick target))

(define-vm-instruction vm-find-package (vm-instruction)
  "Find a package by designator through the runtime package registry."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :find-package)
  (:sexp-slots dst src))

(define-vm-instruction vm-find-symbol (vm-instruction)
  "Find a symbol by name in a package designator."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (pkg nil :reader vm-find-symbol-pkg)
  (:sexp-tag :find-symbol)
  (:sexp-slots dst src pkg))

(define-vm-instruction vm-gensym-inst (vm-instruction)
  "Generate a unique uninterned symbol."
  (dst nil :reader vm-dst)
  (:sexp-tag :gensym)
  (:sexp-slots dst))

(define-vm-instruction vm-keywordp (vm-instruction)
  "Test if value is a keyword symbol. Returns 1 if true, 0 otherwise."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :keywordp)
  (:sexp-slots dst src))

;;; Immediate-symbol fast paths: bypass the general execute-instruction method
;;; when the source value is a packed immediate symbol (vm-immediate-symbol-p).

(defmethod execute-instruction :around ((inst vm-intern-symbol) state pc labels)
  (multiple-value-bind (next-pc value-1 value-2) (call-next-method)
    (when (vm-immediate-intern-enabled-p)
      (let ((value (vm-reg-get state (vm-dst inst))))
        (when (symbolp value)
          (vm-reg-set state (vm-dst inst) (vm-encode-common-symbol value)))))
    (values next-pc value-1 value-2)))

(defmethod execute-instruction :around ((inst vm-keywordp) state pc labels)
  (declare (ignore labels))
  (let ((value (vm-reg-get state (vm-src inst))))
    (if (vm-immediate-symbol-p value)
        (progn
          (vm-reg-set state (vm-dst inst)
                      (if (keywordp (vm-decode-symbol value)) 1 0))
          (values (1+ pc) nil nil))
        (call-next-method))))

(define-simple-instruction vm-symbol-name :unary symbol-name)

;;; Fast path: immediate symbols don't need the general symbol-name path.
(defmethod execute-instruction :around ((inst vm-symbol-name) state pc labels)
  (declare (ignore labels))
  (let ((value (vm-reg-get state (vm-src inst))))
    (if (vm-immediate-symbol-p value)
        (progn
          (vm-reg-set state (vm-dst inst)
                      (rt-string-intern (symbol-name (vm-decode-symbol value))))
          (values (1+ pc) nil nil))
        (call-next-method))))

(defmethod execute-instruction ((inst vm-symbol-name) state pc labels)
  (declare (ignore labels))
  (let ((result (%vm-maybe-sso-string (symbol-name (vm-reg-get state (vm-src inst))))))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(define-simple-instruction vm-make-symbol :unary make-symbol)

(defmethod execute-instruction ((inst vm-intern-symbol) state pc labels)
  (declare (ignore labels))
  (let* ((name (string (%vm-host-string (vm-reg-get state (vm-src inst)))))
         (package-supplied-p (vm-intern-pkg inst))
         (package-designator
           (when package-supplied-p
             (vm-reg-get state (vm-intern-pkg inst))))
         (result
           (cond
             ((not package-supplied-p)
              (cond
                (cl-cc/bootstrap:*runtime-intern-fn*
                 (funcall cl-cc/bootstrap:*runtime-intern-fn* name))
                (*vm-self-host-mode*
                 (error "Package self-hosting required: cannot intern ~S without runtime package registry"
                        name))
                (t
                 (intern name))))
             (t
              (multiple-value-bind (runtime-package host-designator)
                  (%vm-resolve-package-designator package-designator)
                (cond
                  (runtime-package
                   (cond
                     (cl-cc/bootstrap:*runtime-intern-fn*
                      (funcall cl-cc/bootstrap:*runtime-intern-fn*
                               name runtime-package))
                     (*vm-self-host-mode*
                      (error "Package self-hosting required: cannot intern ~S without runtime package registry"
                             name))
                     (t
                      (cl-cc/runtime:rt-intern name runtime-package))))
                  (*vm-self-host-mode*
                   (error "Package self-hosting required: cannot intern ~S without runtime package registry"
                          name))
                  (t
                   (intern name (find-package host-designator)))))))))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-add-package-local-nickname) state pc labels)
  (declare (ignore labels))
  (let* ((pkg-designator (vm-reg-get state (vm-local-nickname-pkg inst)))
         (nickname (vm-reg-get state (vm-local-nickname-nick inst)))
         (target (vm-reg-get state (vm-local-nickname-target inst)))
         (fn (%vm-host-package-local-nickname-function
              "ADD-PACKAGE-LOCAL-NICKNAME")))
    (multiple-value-bind (runtime-target host-target)
        (%vm-resolve-package-designator target)
      (declare (ignore runtime-target))
      (multiple-value-bind (runtime-package host-package)
          (%vm-resolve-package-designator pkg-designator)
        (declare (ignore runtime-package))
        (let ((result (funcall fn nickname host-target host-package)))
          (vm-reg-set state (vm-dst inst) result)
          (values (1+ pc) nil nil))))))

(defmethod execute-instruction ((inst vm-remove-package-local-nickname)
                                  state pc labels)
  (declare (ignore labels))
  (let* ((pkg-designator (vm-reg-get state (vm-local-nickname-pkg inst)))
         (nickname (vm-reg-get state (vm-local-nickname-nick inst)))
         (fn (%vm-host-package-local-nickname-function
              "REMOVE-PACKAGE-LOCAL-NICKNAME")))
    (multiple-value-bind (runtime-package host-package)
        (%vm-resolve-package-designator pkg-designator)
      (declare (ignore runtime-package))
      (let ((result (funcall fn nickname host-package)))
        (vm-reg-set state (vm-dst inst) result)
        (values (1+ pc) nil nil)))))

(defmethod execute-instruction ((inst vm-find-package) state pc labels)
  (declare (ignore labels))
  (multiple-value-bind (runtime-package host-designator)
      (%vm-resolve-package-designator (vm-reg-get state (vm-src inst)))
    (let ((result
            (or runtime-package
                (and *vm-self-host-mode*
                     (not (packagep host-designator))
                     (error "Package self-hosting required: package ~S not found in runtime registry"
                            (vm-reg-get state (vm-src inst))))
                (find-package host-designator))))
      (vm-reg-set state (vm-dst inst) result)
      (values (1+ pc) nil nil))))

(defmethod execute-instruction ((inst vm-find-symbol) state pc labels)
  (declare (ignore labels))
  (let* ((name (string (%vm-host-string (vm-reg-get state (vm-src inst)))))
         (package-supplied-p (vm-find-symbol-pkg inst))
         (package-designator
           (when package-supplied-p
             (vm-reg-get state (vm-find-symbol-pkg inst)))))
    (multiple-value-bind (symbol status)
        (if package-supplied-p
            (multiple-value-bind (runtime-package host-designator)
                (%vm-resolve-package-designator package-designator)
              (if runtime-package
                  (let* ((table (gethash :symbols runtime-package))
                         (exports (gethash :exports runtime-package)))
                    (multiple-value-bind (found present-p)
                        (if table (gethash name table) (values nil nil))
                      (if present-p
                          (values found
                                  (if (member found exports :test (function eq))
                                      :external
                                      :internal))
                          (if (packagep host-designator)
                              (find-symbol name host-designator)
                              (values nil nil)))))
                  (find-symbol name host-designator)))
            (find-symbol name))
      (vm-reg-set state (vm-dst inst) symbol)
      (setf (vm-values-list state) (list symbol status))
      (values (1+ pc) nil nil))))

(defmethod execute-instruction ((inst vm-gensym-inst) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (gensym))
  (values (1+ pc) nil nil))

(define-simple-instruction vm-keywordp :pred1 keywordp)

;;; ─── Extended Package Operation Instructions (ANSI CL Ch.11) ────────────────

(define-vm-instruction vm-export-inst (vm-instruction)
  "Export symbols from a package."
  (dst nil :reader vm-dst)
  (symbols nil :reader vm-export-symbols)
  (pkg nil :reader vm-export-pkg)
  (:sexp-tag :export)
  (:sexp-slots dst symbols pkg))

(defmethod execute-instruction ((inst vm-export-inst) state pc labels)
  (declare (ignore labels))
  (let* ((symbols (vm-reg-get state (vm-export-symbols inst)))
         (pkg (vm-reg-get state (vm-export-pkg inst)))
         (result (%vm-package-operation-result
                  pkg
                  (function cl-cc/runtime::rt-export)
                  (function export)
                  (list symbols pkg))))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(define-vm-instruction vm-import-inst (vm-instruction)
  "Import symbols into a package."
  (dst nil :reader vm-dst)
  (symbols nil :reader vm-import-symbols)
  (pkg nil :reader vm-import-pkg)
  (:sexp-tag :import)
  (:sexp-slots dst symbols pkg))

(defmethod execute-instruction ((inst vm-import-inst) state pc labels)
  (declare (ignore labels))
  (let* ((symbols (vm-reg-get state (vm-import-symbols inst)))
         (pkg (vm-reg-get state (vm-import-pkg inst)))
         (result (%vm-package-operation-result
                  pkg
                  (function cl-cc/runtime::rt-import)
                  (function import)
                  (list symbols pkg))))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(define-vm-instruction vm-use-package-inst (vm-instruction)
  "Make packages accessible to another package."
  (dst nil :reader vm-dst)
  (packages-to-use nil :reader vm-use-packages)
  (pkg nil :reader vm-use-pkg)
  (:sexp-tag :use-package)
  (:sexp-slots dst packages-to-use pkg))

(defmethod execute-instruction ((inst vm-use-package-inst) state pc labels)
  (declare (ignore labels))
  (let* ((pkgs (vm-reg-get state (vm-use-packages inst)))
         (pkg (vm-reg-get state (vm-use-pkg inst)))
         (result (%vm-package-operation-result
                  pkg
                  (function cl-cc/runtime::rt-use-package)
                  (function use-package)
                  (list pkgs pkg)
                  0)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(define-vm-instruction vm-unuse-package-inst (vm-instruction)
  "Remove packages from the use-list of another package."
  (dst nil :reader vm-dst)
  (packages-to-unuse nil :reader vm-unuse-packages)
  (pkg nil :reader vm-unuse-pkg)
  (:sexp-tag :unuse-package)
  (:sexp-slots dst packages-to-unuse pkg))

(defmethod execute-instruction ((inst vm-unuse-package-inst) state pc labels)
  (declare (ignore labels))
  (let* ((pkgs (vm-reg-get state (vm-unuse-packages inst)))
         (pkg (vm-reg-get state (vm-unuse-pkg inst)))
         (result (%vm-package-operation-result
                  pkg
                  (function cl-cc/runtime::rt-unuse-package)
                  (function unuse-package)
                  (list pkgs pkg)
                  0)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(define-vm-instruction vm-shadow-inst (vm-instruction)
  "Create shadowing symbols in a package."
  (dst nil :reader vm-dst)
  (symbol-names nil :reader vm-shadow-names)
  (pkg nil :reader vm-shadow-pkg)
  (:sexp-tag :shadow)
  (:sexp-slots dst symbol-names pkg))

(defmethod execute-instruction ((inst vm-shadow-inst) state pc labels)
  (declare (ignore labels))
  (let* ((names (vm-reg-get state (vm-shadow-names inst)))
         (pkg (vm-reg-get state (vm-shadow-pkg inst)))
         (result (%vm-package-operation-result
                  pkg
                  (function cl-cc/runtime::rt-shadow)
                  (function shadow)
                  (list names pkg))))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(define-vm-instruction vm-unintern-inst (vm-instruction)
  "Remove a symbol from its home package."
  (dst nil :reader vm-dst)
  (symbol nil :reader vm-unintern-symbol)
  (pkg nil :reader vm-unintern-pkg)
  (:sexp-tag :unintern)
  (:sexp-slots dst symbol pkg))

(defmethod execute-instruction ((inst vm-unintern-inst) state pc labels)
  (declare (ignore labels))
  (let* ((sym (vm-reg-get state (vm-unintern-symbol inst)))
         (pkg (vm-reg-get state (vm-unintern-pkg inst)))
         (result (%vm-package-operation-result
                  pkg
                  (function cl-cc/runtime::rt-unintern)
                  (function unintern)
                  (list sym pkg))))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(define-vm-instruction vm-package-name-inst (vm-instruction)
  "Get the name of a package."
  (dst nil :reader vm-dst)
  (pkg nil :reader vm-package-name-pkg)
  (:sexp-tag :package-name)
  (:sexp-slots dst pkg))

(defmethod execute-instruction ((inst vm-package-name-inst) state pc labels)
  (declare (ignore labels))
  (let* ((pkg (vm-reg-get state (vm-package-name-pkg inst)))
         (result (%vm-package-result pkg
                                     #'cl-cc/runtime::rt-package-name
                                     #'package-name
                                     pkg)))
    (vm-reg-set state (vm-dst inst)
                (%vm-maybe-sso-string result))
    (values (1+ pc) nil nil)))

(define-vm-instruction vm-list-all-packages-inst (vm-instruction)
  "List all registered packages."
  (dst nil :reader vm-dst)
  (:sexp-tag :list-all-packages)
  (:sexp-slots dst))

(defmethod execute-instruction ((inst vm-list-all-packages-inst) state pc labels)
  (declare (ignore labels))
  (let ((result (%vm-registry-result #'cl-cc/runtime::rt-list-all-packages
                                     #'list-all-packages)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

;;; ─── Character Predicate Instructions ───────────────────────────────────────

;; define-vm-unary-instruction is defined in vm.lisp (available to all vm/ files).

(define-vm-unary-instruction vm-digit-char-p  :digit-char-p  "Test if character is a digit. Returns weight or nil.")
(define-vm-unary-instruction vm-alpha-char-p  :alpha-char-p  "Test if character is alphabetic. Returns 1/0.")
(define-vm-unary-instruction vm-upper-case-p  :upper-case-p  "Test if character is upper case. Returns 1/0.")
(define-vm-unary-instruction vm-lower-case-p  :lower-case-p  "Test if character is lower case. Returns 1/0.")
(define-vm-unary-instruction vm-char-upcase   :char-upcase   "Upcase a character. DST = uppercase of SRC.")
(define-vm-unary-instruction vm-char-downcase :char-downcase "Downcase a character. DST = lowercase of SRC.")
(define-vm-unary-instruction vm-stringp       :stringp       "Test if value is a string. Returns 1/0.")
(define-vm-unary-instruction vm-characterp    :characterp    "Test if value is a character. Returns 1/0.")
(define-vm-unary-instruction vm-parse-integer :parse-integer "Parse an integer from a string. DST = integer value.")
(define-vm-unary-instruction vm-alphanumericp :alphanumericp "Test if character is alphanumeric. Returns 1/0.")

(defmethod execute-instruction ((inst vm-digit-char-p) state pc labels)
  (declare (ignore labels))
  (let* ((ch (vm-reg-get state (vm-src inst)))
         (result (vm-digit-char-p-value ch)))
    (vm-reg-set state (vm-dst inst) (or result nil))
    (values (1+ pc) nil nil)))

(define-simple-instruction vm-alpha-char-p :pred1 vm-alpha-char-p-value)
(define-simple-instruction vm-upper-case-p :pred1 vm-upper-case-p-value)
(define-simple-instruction vm-lower-case-p :pred1 vm-lower-case-p-value)
(define-simple-instruction vm-char-upcase :unary char-upcase)
(define-simple-instruction vm-char-downcase :unary char-downcase)
(define-simple-instruction vm-stringp :pred1 stringp)
(define-simple-instruction vm-characterp :pred1 characterp)
(define-simple-instruction vm-parse-integer :unary parse-integer)
(define-simple-instruction vm-alphanumericp :pred1 vm-alphanumericp-value)

;;; ─── Sleep (FR-681) ─────────────────────────────────────────────────────────

(define-vm-unary-instruction vm-sleep-inst :sleep "Suspend execution for N seconds.")
(define-simple-instruction vm-sleep-inst :unary sleep)
