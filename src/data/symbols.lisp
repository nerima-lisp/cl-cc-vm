(in-package #:cl-cc/vm/data)

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

(define-simple-instruction vm-symbol-name :unary symbol-name)

(define-simple-instruction vm-make-symbol :unary make-symbol)

(define-simple-instruction vm-keywordp :pred1 keywordp)

;;; ─── Extended Package Operation Instructions (ANSI CL Ch.11) ────────────────

(define-vm-instruction vm-export-inst (vm-instruction)
  "Export symbols from a package."
  (dst nil :reader vm-dst)
  (symbols nil :reader vm-export-symbols)
  (pkg nil :reader vm-export-pkg)
  (:sexp-tag :export)
  (:sexp-slots dst symbols pkg))

(define-vm-instruction vm-import-inst (vm-instruction)
  "Import symbols into a package."
  (dst nil :reader vm-dst)
  (symbols nil :reader vm-import-symbols)
  (pkg nil :reader vm-import-pkg)
  (:sexp-tag :import)
  (:sexp-slots dst symbols pkg))

(define-vm-instruction vm-use-package-inst (vm-instruction)
  "Make packages accessible to another package."
  (dst nil :reader vm-dst)
  (packages-to-use nil :reader vm-use-packages)
  (pkg nil :reader vm-use-pkg)
  (:sexp-tag :use-package)
  (:sexp-slots dst packages-to-use pkg))

(define-vm-instruction vm-unuse-package-inst (vm-instruction)
  "Remove packages from the use-list of another package."
  (dst nil :reader vm-dst)
  (packages-to-unuse nil :reader vm-unuse-packages)
  (pkg nil :reader vm-unuse-pkg)
  (:sexp-tag :unuse-package)
  (:sexp-slots dst packages-to-unuse pkg))

(define-vm-instruction vm-shadow-inst (vm-instruction)
  "Create shadowing symbols in a package."
  (dst nil :reader vm-dst)
  (symbol-names nil :reader vm-shadow-names)
  (pkg nil :reader vm-shadow-pkg)
  (:sexp-tag :shadow)
  (:sexp-slots dst symbol-names pkg))

(define-vm-instruction vm-unintern-inst (vm-instruction)
  "Remove a symbol from its home package."
  (dst nil :reader vm-dst)
  (symbol nil :reader vm-unintern-symbol)
  (pkg nil :reader vm-unintern-pkg)
  (:sexp-tag :unintern)
  (:sexp-slots dst symbol pkg))

(define-vm-instruction vm-package-name-inst (vm-instruction)
  "Get the name of a package."
  (dst nil :reader vm-dst)
  (pkg nil :reader vm-package-name-pkg)
  (:sexp-tag :package-name)
  (:sexp-slots dst pkg))

(define-vm-instruction vm-list-all-packages-inst (vm-instruction)
  "List all registered packages."
  (dst nil :reader vm-dst)
  (:sexp-tag :list-all-packages)
  (:sexp-slots dst))

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

(define-simple-instruction vm-alpha-char-p :pred1 vm-alpha-char-p-value)
(define-simple-instruction vm-upper-case-p :pred1 vm-upper-case-p-value)
(define-simple-instruction vm-lower-case-p :pred1 vm-lower-case-p-value)
(define-simple-instruction vm-char-upcase :unary char-upcase)
(define-simple-instruction vm-char-downcase :unary char-downcase)
(define-simple-instruction vm-stringp :pred1 stringp)
(define-simple-instruction vm-characterp :pred1 characterp)
(define-simple-instruction vm-parse-integer :unary parse-integer)
(define-simple-instruction vm-alphanumericp :pred1 vm-alphanumericp-value)

