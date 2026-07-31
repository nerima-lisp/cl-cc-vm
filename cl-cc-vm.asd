;;;; cl-cc-vm.asd — the cl-cc bytecode virtual machine
;;;;
;;;; Extracted from the cl-cc monorepo. cl-cc's own design document had ruled
;;;; this out of scope, on the grounds that the core is a self-referential web,
;;;; citing 301 cross-package `cl-cc/vm::` references. That count points the
;;;; wrong way: it measures other packages reaching *into* the VM's internals,
;;;; not the VM depending on anything. The first is a public API problem, and
;;;; cl-cc/optimize and cl-cc/codegen reached zero such references before this
;;;; extraction.
;;;;
;;;; What the VM actually depends on is what is declared below: cl-cc-bootstrap
;;;; and cl-cc-runtime. Loading it leaves cl-cc/expand, cl-cc/compile,
;;;; cl-cc/optimize, cl-cc/parse, cl-cc/ast and cl-cc/type unloaded.

(asdf:defsystem :cl-cc-vm
  :description "VM instruction set, executor, I/O, CLOS, conditions, collections"
  :author "takeokunn <bararararatty@gmail.com>"
  :maintainer "takeokunn <bararararatty@gmail.com>"
  :license "MIT"
  :homepage "https://github.com/nerima-lisp/cl-cc-vm"
  :bug-tracker "https://github.com/nerima-lisp/cl-cc-vm/issues"
  :source-control (:git "https://github.com/nerima-lisp/cl-cc-vm.git")
  :version "0.1.0"
  :depends-on (:cl-cc-bootstrap :cl-cc-runtime :cl-regex-kit :cl-tty-kit)
  :in-order-to ((asdf:test-op (asdf:test-op "cl-cc-vm/test")))
  :pathname "src"
  :serial t
  :components
   ((:file "facade-package-defpackage")
    (:file "exports-instructions")
    (:file "exports-instructions-constructors-core")
    (:file "exports-instructions-constructors-io")
    (:file "exports-runtime")
    (:file "exports-runtime-objects")
    (:file "exports-opcodes")
   (:file "exports-conditions")
   (:file "package")
    (:file "vm-fasl")
    (:file "vm")
    (:file "vm-environment-introspection")
    (:file "vm-readtable")
    (:file "vm-dsl")
    (:file "vm-symbol-table")
    (:file "vm-tier")
    (:file "vm-state-init")
   (:file "vm-bridge")
   (:file "vm-bridge-io-docs")
   (:file "vm-instructions")
   (:file "vm-dispatch")
   (:file "vm-dispatch-gf")
   (:file "vm-dispatch-gf-multi")
   (:file "vm-dispatch-gf-call")
   (:file "vm-execute-safety")
   (:file "vm-execute-numeric")
   (:file "vm-execute-osr")
   (:file "vm-execute")
    (:file "vm-execute-mv")
    (:file "vm-sequence")
    (:file "vm-clos")
    (:file "vm-clos-mop")
    (:file "vm-clos-instance-cache")
    (:file "vm-ic")
    (:file "vm-clos-slots-storage")
    (:file "vm-clos-slots-introspection")
    (:file "vm-clos-slots")
    (:file "vm-clos-execute")
   (:file "vm-run")
    (:file "vm-opcodes")
    (:file "vm-opcodes-shapes")
    (:file "vm-opcodes-defs")
    (:file "vm-opcodes-run")
    (:file "primitives")
    (:file "primitives-float-precision")
    (:file "primitives-numeric-dispatch")
    (:file "primitives-typep")
    (:file "primitives-numeric-fp-traps")
   (:file "vm-bitwise")
   (:file "vm-transcendental")
     (:file "vm-numeric")
     (:file "vm-numeric-bignum-algorithms")
     (:file "vm-numeric-complex")
     (:file "vm-numeric-bignum")
     (:file "vm-numeric-rational")
     (:file "vm-numeric-complex-native")
     (:file "vm-numeric-tower")
     (:file "vm-environment")
     (:file "time")
    (:file "random")
    (:file "vm-numeric-ext")
   (:file "vm-extensions")
    (:file "io-external-format")
    (:file "io")
    (:file "io-network")
    (:file "print-circle")
    (:file "io-instructions")
   (:file "io-execute")
    (:file "io-predicates")
    (:file "io-runners")
     (:file "pathname")
       (:file "stream")
                               (:file "string-builder")
       (:file "ryu")
       (:file "format-render-directives")
       (:file "format-render")
       (:file "format")
     (:file "pprint")
      (:file "condition-types")
      (:file "condition-handler-stack")
      (:file "vm-exception-tables")
      (:file "conditions")
   (:file "conditions-instructions")
   (:file "list-coerce")
   (:file "list-hash-cons")
   (:file "list-managed-cons")
   (:file "list")
   (:file "list-execute")
   (:file "array-storage")
   (:file "array")
   (:file "array-bits")
     (:file "strings-storage")
     (:file "strings-char-classes")
     (:file "strings")
    (:file "strings-char-instrs")
    (:file "unicode")
    (:file "symbols-packages")
    (:file "symbols")
    (:file "hash")
    (:file "hash-execute")
     (:file "atomics")
     ;; ── Phase 129-160: Advanced Compilation III ──
     (:file "strings-optimize-130")
     (:file "v8-objects-133")
     (:file "security-134")
     (:file "stack-thread-137")
     (:file "numeric-fp-141")
     (:file "phases-142-144")
     (:file "phases-145-147")
     (:file "phases-148-150")
     (:file "phases-151-153")
     (:file "phases-154-156")
     (:file "phases-157-160")
      (:file "persistent")
      (:file "json")
      (:file "regex")
      (:file "runtime-stdlib-2-completion")
      (:file "runtime-stdlib-3")
       (:file "vm-bytecode-stdlib3")
       (:file "vm-dynamic-wind")
        (:file "vm-source-tracking")
        (:file "vm-debug")
        (:file "vm-ryu")
       (:file "vm-terminal")
       (:file "crash-report")
       (:file "vm-serialize")))

;;;; ---------------------------------------------------------------------
;;;; Tests — run with (asdf:test-system :cl-cc-vm) or :cl-cc-vm/tests.
;;;; The monorepo's cl-cc-vm/tests system is deliberately not carried over. It
;;;; depended on :cl-cc-testing-framework, which exists only inside cl-cc, and
;;;; its tests live on there as packages/vm/tests -- they exercise this system
;;;; through the flake input, which is the arrangement every extracted package
;;;; uses. What is here instead is a boundary suite: it asserts the dependency
;;;; closure and the public surface this extraction rests on.

(asdf:defsystem "cl-cc-vm/test"
  :description "Module boundary tests for cl-cc-vm"
  :author "takeokunn <bararararatty@gmail.com>"
  :license "MIT"
  :depends-on ("cl-cc-vm" "cl-weave")
  :pathname "t"
  :serial t
  :components ((:file "package")
               (:file "vm-boundary-test")
               (:file "regex-test")
               (:file "vm-numeric-bignum-test")
               (:file "vm-format-render-test")
               (:file "vm-terminal-test")
               (:file "vm-list-execute-test")
               (:file "vm-array-execute-test")
               (:file "vm-hash-execute-test")
               (:file "vm-string-execute-test")
               (:file "vm-clos-execute-test"))
  :perform (asdf:test-op (op system)
             (declare (ignore op system))
             (uiop:symbol-call :cl-weave :run-all-tests :pass-with-no-tests nil)))
