# Glossary

## protocols

- Definition: Shared contracts that let VM features call one another without depending on implementation packages.
- Package: `cl-cc/vm/protocols`
- Key types: `vm-instruction`
- Key functions: `execute-instruction`, `vm-reg-get`, `vm-lookup-bridge-callable`, `vm-register-host-bridge`, `vm-register-runtime-callable`
- Other contexts: None.

## execution

- Definition: VM state, instruction execution, dispatch, and control flow.
- Package: `cl-cc/vm/execution`
- Key types: `vm-state`, `vm-program`, `vm-closure-object`
- Key functions: `make-vm-state`, `run-compiled`, `run-program-slice`
- Other contexts: None.

## numbers

- Definition: VM numeric representations, arithmetic operations, numeric instructions, and conversions.
- Package: `cl-cc/vm/numbers`
- Key types: `double-double`, `interval`
- Key functions: `fixnum-add`, `fixnum-mul`, `interval-add`
- Other contexts: None.

## clos

- Definition: VM integration with the Common Lisp object system, generic functions, methods, and slot access.
- Package: `cl-cc/vm/clos`
- Key types: `vm-find-class`, `vm-generic-call`, `vm-register-method`
- Key functions: `collect-inherited-slots`, `compute-class-precedence-list`, `vm-classify-arg`
- Other contexts: The host CLOS defines generic functions and methods; this feature implements their VM behavior.

## conditions

- Definition: VM condition types, handler and restart stacks, exception tables, and dynamic control transfer.
- Package: `cl-cc/vm/conditions`
- Key types: `vm-condition`, `vm-simple-condition`
- Key functions: `vm-signal-condition`, `vm-find-handler`, `vm-find-restart`
- Other contexts: Common Lisp conditions are host objects; these types model guest VM conditions.

## data

- Definition: VM collection and value representations for arrays, lists, hashes, strings, symbols, and sequences.
- Package: `cl-cc/vm/data`
- Key types: `vm-hash-table-object`, `vm-make-array`, `vm-make-hash-table`, `vm-specialized-array`, `vm-string-length`
- Key functions: `vm-make-specialized-array`, `vm-sequence-elt`, `vm-sequence-length`
- Other contexts: None.

## io

- Definition: VM stream state, input and output operations, formatting, pathnames, printing, and reader behavior.
- Package: `cl-cc/vm/io`
- Key types: `vm-io-state`
- Key functions: `read-balanced-form`, `vm-file-position`, `vm-open`
- Other contexts: Host streams are adapted to VM stream state and guest-visible I/O.

## serialization

- Definition: Persistent encodings for VM programs and values, including FASL load and save operations.
- Package: `cl-cc/vm/serialization`
- Key types: None.
- Key functions: `vm-write-to-fasl`, `vm-read-from-fasl`, `vm-load-fasl`
- Other contexts: None.

## environment

- Definition: Host process, time, random-number, build, and environment-variable interfaces exposed to the VM.
- Package: `cl-cc/vm/environment`
- Key types: None.
- Key functions: `getenv`, `sleep`, `random`
- Other contexts: Host environment values are obtained through this boundary; VM data representations remain in `data`.

## debugging

- Definition: VM diagnostics, backtraces, terminal interaction, profiling, and debugger behavior.
- Package: `cl-cc/vm/debugging`
- Key types: None.
- Key functions: `vm-invoke-debugger`, `vm-step`, `vm-terminal-size`
- Other contexts: None.

## interop

- Definition: Adapters for calling and exposing host functions, regular expressions, FFI, and sandboxed integrations.
- Package: `cl-cc/vm/interop`
- Key types: None.
- Key functions: `regex-scan`, `regex-replace`, `regex-replace-all`
- Other contexts: Host package behavior remains outside the VM and crosses this adapter boundary.
