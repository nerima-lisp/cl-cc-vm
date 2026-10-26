(
  (:feature "protocols"
    :source-dirs ("src/protocols")
    :test-dir "t/protocols"
    :facade-packages ("cl-cc/vm")
    :domain-package "cl-cc/vm/protocols"
    :infra-package nil
    :depends-on ())

  ; execution -> protocols: vm-feature-initial-globals, src/execution/vm-state-init.lisp:217-219.
  ; execution -> data: data instruction methods, src/execution/data-instruction-methods.lisp:8.
  (:feature "execution"
    :source-dirs ("src/execution")
    :test-dir "t/execution"
    :facade-packages ("cl-cc/vm")
    :domain-package "cl-cc/vm/execution"
    :infra-package nil
    :depends-on ("protocols" "data"))

  ; data -> protocols: host bridge registration, src/data/strings.lisp:12-17.
  (:feature "data"
    :source-dirs ("src/data")
    :test-dir "t/data"
    :facade-packages ("cl-cc/vm")
    :domain-package "cl-cc/vm/data"
    :infra-package nil
    :depends-on ("protocols"))

  ; numbers -> protocols/environment: env-query macro call, src/numbers/vm-numeric-ext.lisp:159-169.
  (:feature "numbers"
    :source-dirs ("src/numbers")
    :test-dir "t/numbers"
    :facade-packages ("cl-cc/vm")
    :domain-package "cl-cc/vm/numbers"
    :infra-package nil
    :depends-on ("protocols" "environment"))

  ; environment -> execution/data/protocols: src/environment/vm-environment.lisp:274; src/environment/random.lisp:15-19,84-85; src/environment/time.lisp:75.
  (:feature "environment"
    :source-dirs ("src/environment")
    :test-dir "t/environment"
    :facade-packages ("cl-cc/vm")
    :domain-package "cl-cc/vm/environment"
    :infra-package nil
    :depends-on ("protocols" "execution" "data"))

  ; conditions -> execution/environment/io/protocols: src/conditions/conditions.lisp:55-56,125; src/conditions/condition-types.lisp:149,157; src/conditions/conditions-instructions.lisp:83-99.
  (:feature "conditions"
    :source-dirs ("src/conditions")
    :test-dir "t/conditions"
    :facade-packages ("cl-cc/vm")
    :domain-package "cl-cc/vm/conditions"
    :infra-package nil
    :depends-on ("protocols" "execution" "environment" "io"))

  ; clos -> execution/data/protocols: src/clos/vm-clos-execute.lisp:22; src/clos/vm-clos-slots-storage.lisp:125.
  (:feature "clos"
    :source-dirs ("src/clos")
    :test-dir "t/clos"
    :facade-packages ("cl-cc/vm")
    :domain-package "cl-cc/vm/clos"
    :infra-package nil
    :depends-on ("protocols" "execution" "data"))

  ; io -> execution/data/numbers/environment/protocols: src/io/io-runners.lisp:24; src/io/io.lisp:138; src/io/format-render-directives.lisp:236; src/io/format.lisp:268.
  (:feature "io"
    :source-dirs ("src/io")
    :test-dir "t/io"
    :facade-packages ("cl-cc/vm")
    :domain-package "cl-cc/vm/io"
    :infra-package nil
    :depends-on ("protocols" "execution" "data" "numbers" "environment"))

  ; serialization -> execution/clos/io/environment/protocols: src/serialization/vm-serialize.lisp:23-24,50,58; src/serialization/vm-fasl.lisp:64.
  (:feature "serialization"
    :source-dirs ("src/serialization")
    :test-dir "t/serialization"
    :facade-packages ("cl-cc/vm")
    :domain-package "cl-cc/vm/serialization"
    :infra-package nil
    :depends-on ("protocols" "execution" "clos" "io" "environment"))

  ; debugging -> execution/conditions/io/environment/protocols: src/debugging/vm-debug.lisp:100,103,166,179.
  (:feature "debugging"
    :source-dirs ("src/debugging")
    :test-dir "t/debugging"
    :facade-packages ("cl-cc/vm")
    :domain-package "cl-cc/vm/debugging"
    :infra-package nil
    :depends-on ("protocols" "execution" "conditions" "io" "environment"))

  ; interop -> execution/clos/debugging/data/io/protocols: src/interop/vm-bridge.lisp:57,155,255,312,322-323; src/interop/vm-bridge-io-docs.lisp:304,349.
  (:feature "interop"
    :source-dirs ("src/interop")
    :test-dir "t/interop"
    :facade-packages ("cl-cc/vm")
    :domain-package "cl-cc/vm/interop"
    :infra-package nil
    :depends-on ("protocols" "execution" "clos" "debugging" "data" "io"))
)
