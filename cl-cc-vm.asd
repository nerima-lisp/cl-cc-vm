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

;;;; This form comes FIRST, before any defsystem. ASDF binds *package* to
;;;; ASDF-USER only for a file it loads itself; read any other way — a REPL
;;;; `load`, an editor evaluating the buffer, flake.nix parsing :version — the
;;;; file is read in whatever package happens to be current.

(in-package #:asdf-user)

;;;; System names are written as STRINGS, not keywords: a string does not
;;;; depend on the reader's current package state, and it is what lets the org
;;;; conformance checker locate this form at all.
(asdf:defsystem "cl-cc-vm"
  :description "VM instruction set, executor, I/O, CLOS, conditions, collections"
  :author "takeokunn <bararararatty@gmail.com>"
  :maintainer "takeokunn <bararararatty@gmail.com>"
  :license "MIT"
  :version "0.1.1"
  :homepage "https://github.com/nerima-lisp/cl-cc-vm"
  :bug-tracker "https://github.com/nerima-lisp/cl-cc-vm/issues"
  :source-control (:git "https://github.com/nerima-lisp/cl-cc-vm.git")
  :depends-on (:cl-cc-bootstrap :cl-cc-runtime :cl-regex-kit :cl-json-kit :cl-tty-kit :cl-host-kit)
  :pathname "src"
  :serial t
  :components
   ((:module "feature-packages" :pathname "." :serial t :components
 ((:module "protocols" :pathname "protocols" :serial t :components ((:file "package")))
  (:module "data" :pathname "data" :serial t :components ((:file "package")))
  (:module "execution" :pathname "execution" :serial t :components ((:file "package")))
  (:module "environment" :pathname "environment" :serial t :components ((:file "package")))
  (:module "numbers" :pathname "numbers" :serial t :components ((:file "package")))
  (:module "io" :pathname "io" :serial t :components ((:file "package")))
  (:module "conditions" :pathname "conditions" :serial t :components ((:file "package")))
  (:module "clos" :pathname "clos" :serial t :components ((:file "package")))
  (:module "serialization" :pathname "serialization" :serial t :components ((:file "package")))
  (:module "debugging" :pathname "debugging" :serial t :components ((:file "package")))
  (:module "interop" :pathname "interop" :serial t :components ((:file "package")))))
 (:file "facade-package-defpackage")
 (:file "package")
 (:module "protocols" :pathname "protocols" :serial t :components
  ((:file "vm-clos-contracts") (:file "vm-condition-contracts")
   (:file "vm-data-contracts")
   (:file "vm-debugger-request")
   (:file "vm-environment-contracts")
   (:file "vm-host-bridges")
   (:file "vm-instruction-contracts")
   (:file "vm-integration-hooks")
   (:file "vm-io-contracts")
   (:file "vm-label-table-contract")
   (:file "vm-runtime-contracts")))
 (:module "data" :pathname "data" :serial t :components
  ((:file "vm-sequence") (:file "string-builder")
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
   (:file "persistent")
   (:file "hash-instructions")
   (:file "heterogeneous-lists")
   (:file "random-state")
   (:file "simd-string-search")
   (:file "string-interpolation-state")
   (:file "strings-optimization")
   (:file "unicode-normalization-api")
   (:file "universal-hashing")
   (:file "vm-reference-counting")
   (:file "vm-symbol-properties")
   (:file "vm-type-representations")))
 (:module "execution" :pathname "execution" :serial t :components
  ((:file "vm") (:file "vm-dsl")
   (:file "vm-tier")
   (:file "vm-state-init")
   (:file "vm-instructions")
   (:file "vm-dispatch")
   (:file "vm-execute-safety")
   (:file "vm-execute-osr")
   (:file "vm-execute")
   (:file "vm-execute-mv")
   (:file "vm-run")
   (:file "vm-opcodes")
   (:file "vm-opcodes-shapes")
   (:file "vm-opcodes-defs")
   (:file "vm-opcodes-run")
   (:file "atomics")
   (:file "vm-bytecode-stdlib3")
   (:file "adaptive-jit")
   (:file "aot-constant-blinding")
   (:file "boolean-instructions")
   (:file "comparison-instructions")
   (:file "compiler-assertions")
   (:file "coroutines")
   (:file "data-instruction-methods")
   (:file "data-instruction-methods-2")
   (:file "data-instruction-methods-3")
   (:file "do-notation")
   (:file "evaluation-instructions")
   (:file "function-specialization")
   (:file "generator-syntax")
   (:file "instruction-optimization")
   (:file "jit-hardening")
   (:file "list-comprehensions")
   (:file "stack-guards")
   (:file "stack-segments")
   (:file "staged-metaprogramming")
   (:file "trampolines")
   (:file "type-forms")
   (:file "type-operations")
   (:file "type-system-syntax")
   (:file "view-patterns")
   (:file "vm-atomic-128")
   (:file "vm-call-context")
   (:file "vm-callcc")
   (:file "vm-continuations")
   (:file "vm-control-flow")
   (:file "vm-forward-references")
   (:file "vm-load-time-value")
   (:file "vm-locks")
   (:file "vm-multiple-values")
   (:file "vm-pass-management")
   (:file "vm-progv")))
 (:module "environment" :pathname "environment" :serial t :components
  ((:file "vm-environment-introspection") (:file "vm-symbol-table")
   (:file "vm-environment")
   (:file "time")
   (:file "random")
   (:file "vm-process-state")
   (:file "vm-source-tracking")
   (:file "build-management")
   (:file "cryptographic-random")
   (:file "feature-flags")
   (:file "library-targets")
   (:file "module-declarations")
   (:file "runtime-tuning")
   (:file "source-spans")
   (:file "vm-build-metadata")
   (:file "vm-symbol-values")
   (:file "vm-thread-local-storage")))
 (:module "numbers" :pathname "numbers" :serial t :components
  ((:file "vm-numeric-instructions") (:file "primitives-float-precision")
   (:file "primitives-numeric-dispatch")
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
   (:file "vm-numeric-ext")
   (:file "ryu")
   (:file "vm-ryu")
   (:file "numeric-comparison-instructions")
   (:file "numeric-floating-point")
   (:file "numeric-predicates")
   (:file "primitive-arithmetic-instructions")
   (:file "primitives-numeric")
   (:file "vm-arithmetic")
   (:file "vm-fixnum")
   (:file "vm-numeric-extensions")
   (:file "vm-runtime-aliases")
   (:file "vm-runtime-support")
   (:file "vm-swar")
   (:file "vm-vector-permutation")))
 (:module "io" :pathname "io" :serial t :components
  ((:file "vm-readtable") (:file "io-external-format")
   (:file "io")
   (:file "io-network")
   (:file "print-circle")
   (:file "io-instructions")
   (:file "io-execute")
   (:file "io-predicates")
   (:file "io-runners")
   (:file "pathname")
   (:file "stream")
   (:file "format-render-directives")
   (:file "format-render")
   (:file "format")
   (:file "pprint")
   (:file "os-operations")
   (:file "read-balanced-form")
   (:file "runtime-external-format")
   (:file "session-types")
   (:file "vm-file-position")))
 (:module "conditions" :pathname "conditions" :serial t :components
  ((:file "condition-types") (:file "condition-handler-stack") (:file "vm-exception-tables") (:file "conditions") (:file "conditions-instructions") (:file "vm-dynamic-wind") (:file "continuation-marks") (:file "vm-check-type")))
 (:module "clos" :pathname "clos" :serial t :components
  ((:file "vm-dispatch-gf") (:file "vm-dispatch-gf-multi")
   (:file "vm-dispatch-gf-call")
   (:file "vm-clos")
   (:file "vm-clos-mop")
   (:file "vm-clos-instance-cache")
   (:file "vm-ic")
   (:file "vm-clos-slots-storage")
   (:file "vm-clos-slots-introspection")
   (:file "vm-clos-slots")
   (:file "vm-clos-execute")
   (:file "vm-copy-instance")))
 (:module "serialization" :pathname "serialization" :serial t :components
  ((:file "vm-fasl") (:file "json") (:file "vm-serialize")))
 (:module "debugging" :pathname "debugging" :serial t :components
  ((:file "vm-debug") (:file "vm-terminal")
   (:file "crash-report")
   (:file "binary-analysis")
   (:file "documentation-tools")
   (:file "macro-expansion")
   (:file "profiling-tools")
   (:file "regression-bisection")
   (:file "repl-tools")
   (:file "stack-overflow")
   (:file "vm-value-profiling")))
 (:module "interop" :pathname "interop" :serial t :components
  ((:file "vm-bridge") (:file "vm-bridge-io-docs") (:file "regex") (:file "c-ffi-support") (:file "code-signing") (:file "sandbox") (:file "vm-hardware-cryptography") (:file "vm-pinned-objects")))
 (:file "exports-instructions")
 (:file "exports-instructions-constructors-core")
 (:file "exports-instructions-constructors-io")
 (:file "exports-runtime")
 (:file "exports-runtime-objects")
 (:file "exports-opcodes")
 (:file "exports-conditions") )
  :in-order-to ((asdf:test-op (asdf:test-op "cl-cc-vm/test") (asdf:test-op "cl-cc-vm/facade-test"))))

;;;; ---------------------------------------------------------------------
;;;; Tests — run with (asdf:test-system "cl-cc-vm").
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
 (:file "vm-package-designator-test")
 (:file "vm-hash-execute-test")
 (:file "vm-string-execute-test")
 (:file "vm-clos-execute-test")
 (:module "support"
 :pathname "support"
 :serial t
 :components ((:file "package")
              (:file "vm-test-support")))
 (:file "cross-feature-tests")
 (:module "protocols"
 :pathname "protocols"
 :serial t
 :components
 (  (:file "package")
  (:file "feature-api-test")
  (:file "vm-execution-protocol-tests")
  (:file "vm-host-bridge-tests")
  (:file "suite")))
 (:module "execution"
 :pathname "execution"
 :serial t
 :components
 (  (:file "package")
  (:file "feature-api-test")
  (:file "package-tests")
  (:file "primitives-typep-tests")
  (:file "runtime-stdlib-2-vm-tests")
  (:file "vm-call-tests")
  (:file "vm-dispatch-tests")
  (:file "vm-execute-tests")
  (:file "vm-execute-tests-2")
  (:file "vm-instructions-tests")
  (:file "vm-opcodes-defs-tests")
  (:file "vm-opcodes-tests")
  (:file "vm-run-fusion-tests")
  (:file "vm-run-tests")
  (:file "vm-run-control-data-tests")
  (:file "vm-run-data-compare-tests")
  (:file "vm-profile-tests")
  (:file "vm-boundary-tests")
  (:file "vm-symbol-representation-tests")
  (:file "vm-tests")
  (:file "suite")))
 (:module "data"
 :pathname "data"
 :serial t
 :components
 (  (:file "package")
  (:file "array-tests")
  (:file "feature-api-test")
  (:file "vm-regression-tests")
  (:file "hash-tests")
  (:file "list-alist-tests")
  (:file "list-coerce-tests")
  (:file "list-tests")
  (:file "persistent-tests")
  (:file "primitives-tests")
  (:file "string-builder-tests")
  (:file "strings-tests")
  (:file "symbols-tests")
  (:file "vm-execution-moved-tests")
  (:file "vm-extensions-tests")
  (:file "vm-sequence-tests")
  (:file "suite")))
 (:module "numbers"
 :pathname "numbers"
 :serial t
 :components
 (  (:file "package")
  (:file "feature-api-test")
  (:file "vm-regression-tests")
  (:file "parse-float-tests")
  (:file "primitives-tests")
  (:file "runtime-stdlib-3-numeric-tests")
  (:file "runtime-stdlib-3-ryu-tests")
  (:file "vm-bignum-tests")
  (:file "vm-bitwise-tests")
  (:file "vm-extensions-tests")
  (:file "vm-numeric-tests")
  (:file "vm-runtime-stdlib-2-smoke-tests")
  (:file "vm-transcendental-tests")
  (:file "suite")))
 (:module "environment"
 :pathname "environment"
 :serial t
 :components
 (  (:file "package")
  (:file "feature-api-test")
  (:file "runtime-stdlib-2-completion-tests")
  (:file "runtime-stdlib-3-tests")
  (:file "vm-environment-tests")
  (:file "vm-runtime-stdlib-2-smoke-tests")
  (:file "suite")))
 (:module "conditions"
 :pathname "conditions"
 :serial t
 :components
 (  (:file "package")
  (:file "conditions-tests")
  (:file "feature-api-test")
  (:file "runtime-stdlib-3-condition-tests")
  (:file "suite")))
 (:module "clos"
 :pathname "clos"
 :serial t
 :components
 (  (:file "package")
  (:file "feature-api-test")
  (:file "vm-clos-execute-extra-tests")
  (:file "vm-clos-execute-tests")
  (:file "vm-clos-tests")
  (:file "vm-clos-dispatch-regression-tests")
  (:file "vm-copy-instance-tests")
  (:file "deep-copy-tests")
  (:file "vm-dispatch-gf-multi-tests")
  (:file "vm-dispatch-gf-tests")
  (:file "vm-execution-moved-tests")
  (:file "vm-mop-tests")
  (:file "suite")))
 (:module "io"
 :pathname "io"
 :serial t
 :components
 (  (:file "package")
  (:file "feature-api-test")
  (:file "format-tests")
  (:file "io-network-tests")
  (:file "io-runners-tests")
  (:file "io-tests")
  (:file "vm-stream-bridge-regression-tests")
  (:file "runtime-stdlib-2-print-circle-tests")
  (:file "runtime-stdlib-3-external-format-tests")
  (:file "runtime-stdlib-3-io-tests")
  (:file "vm-runtime-stdlib-2-smoke-tests")
  (:file "suite")))
 (:module "serialization"
 :pathname "serialization"
 :serial t
 :components
 (  (:file "package")
  (:file "feature-api-test")
  (:file "json-test")
  (:file "json-tests")
  (:file "runtime-stdlib-3-fasl-tests")
  (:file "vm-runtime-stdlib-2-smoke-tests")
  (:file "suite")))
 (:module "debugging"
 :pathname "debugging"
 :serial t
 :components
 (  (:file "package")
  (:file "crash-report-tests")
  (:file "feature-api-test")
  (:file "runtime-stdlib-3-debug-tests")
  (:file "runtime-stdlib-3-terminal-tests")
  (:file "vm-debugging-moved-tests")
  (:file "suite")))
 (:module "interop"
 :pathname "interop"
 :serial t
 :components
 (  (:file "package")
  (:file "feature-api-test")
  (:file "regex-tests")
  (:file "vm-bridge-tests")
  (:file "suite"))))
  ;; Not HOST-KIT:SYMBOL-CALL: a .asd is read before :depends-on is ever
  ;; consulted, so a CL-HOST-KIT-prefixed token here would be a read-time
  ;; PACKAGE-DOES-NOT-EXIST error regardless of what the system depends on.
  ;; FIND-SYMBOL/FIND-PACKAGE/FUNCALL are CL, always present.
  :perform (asdf:test-op (op system) (declare (ignore op system)) (cl-cc-vm-run-test-system "cl-cc-vm/test")))

(defun cl-cc-vm-test-source-pathnames (component) (if (typep component (quote asdf:source-file)) (list (asdf:component-pathname component)) (mapcan (function cl-cc-vm-test-source-pathnames) (asdf:component-children component))))

(asdf:defsystem "cl-cc-vm/facade-test"
  :description "Facade symbol identity tests"
  :depends-on ("cl-cc-vm" "cl-weave")
  :pathname "t/facade"
  :serial t
  :components ((:file "package") (:file "vm-facade-test"))
  :perform (asdf:test-op (op system)
                         (declare (ignore op system))
                         (cl-cc-vm-run-test-system "cl-cc-vm/facade-test")))

(defun cl-cc-vm-run-test-system (system-name)
  (let* ((system (asdf:find-system system-name))
         (location-filter (cl-cc-vm-test-source-pathnames system))
         (list-tests (find-symbol "LIST-TESTS" (find-package "CL-WEAVE")))
         (run-all (find-symbol "RUN-ALL" (find-package "CL-WEAVE"))))
    (let ((test-count
           (length
            (funcall list-tests
                     :stream
                     (make-broadcast-stream)
                     :location-filter
                     location-filter))))
      (format *standard-output*
              "~&~A: running ~D test~:P~%"
              system-name
              test-count)
      (finish-output)
      (unless
          (funcall run-all
                   :location-filter
                   location-filter
                   :pass-with-no-tests
                   nil)
        (error "cl-cc-vm test suite failed for ~A" system-name))
      (format *standard-output*
              "~&~A: PASSED (~D test~:P)~%"
              system-name
              test-count)
      (finish-output))))
