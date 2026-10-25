# Glossary

## protocols

- Definition: Shared contracts that let VM features call one another without depending on implementation packages.
- Package: `cl-cc/vm/protocols`
- Key types: `vm-instruction`
- Key functions: `execute-instruction`, `vm-reg-get`, `register-vm-host-bridge`
- Other contexts: None.

## execution

- Definition: VM state, instruction execution, dispatch, and control flow.
- Package: `cl-cc/vm/execution`
- Key types: `vm-state`, `vm-program`, `vm-closure-object`
- Key functions: `make-vm-state`, `vm-run`, `vm-dispatch`
- Other contexts: None.

## numbers

- Definition: VM numeric representations, arithmetic operations, numeric instructions, and conversions.
- Package: `cl-cc/vm/numbers`
- Key types: None.
- Key functions: `vm-arithmetic`, `vm-numeric`, `vm-numeric-bignum`
- Other contexts: None.

## clos

- Definition: VM integration with the Common Lisp object system, generic functions, methods, and slot access.
- Package: `cl-cc/vm/clos`
- Key types: `vm-generic-call`
- Key functions: `vm-dispatch-gf`, `vm-copy-instance`
- Other contexts: The host CLOS defines generic functions and methods; this feature implements their VM behavior.

## conditions

- Definition: VM condition types, handler and restart stacks, exception tables, and dynamic control transfer.
- Package: `cl-cc/vm/conditions`
- Key types: `vm-condition`, `vm-handler`, `vm-restart`
- Key functions: `vm-signal-condition`, `vm-find-handler`, `vm-find-restart`
- Other contexts: Common Lisp conditions are host objects; these types model guest VM conditions.

## data

- Definition: VM collection and value representations for arrays, lists, hashes, strings, symbols, and sequences.
- Package: `cl-cc/vm/data`
- Key types: `vm-array`, `vm-hash-table`, `vm-string`
- Key functions: `vm-cons`, `vm-hash-cons`, `vm-string`
- Other contexts: None.

## io

- Definition: VM stream state, input and output operations, formatting, pathnames, printing, and reader behavior.
- Package: `cl-cc/vm/io`
- Key types: `vm-io-state`
- Key functions: `vm-format`, `vm-read`, `vm-write`
- Other contexts: Host streams are adapted to VM stream state and guest-visible I/O.

## serialization

- Definition: Persistent encodings for VM programs and values, including FASL load and save operations.
- Package: `cl-cc/vm/serialization`
- Key types: None.
- Key functions: `vm-serialize`, `vm-read-from-fasl`, `vm-load-fasl`
- Other contexts: None.

## environment

- Definition: Host process, time, random-number, build, and environment-variable interfaces exposed to the VM.
- Package: `cl-cc/vm/environment`
- Key types: None.
- Key functions: `vm-getenv`, `vm-sleep`, `vm-random`
- Other contexts: Host environment values are obtained through this boundary; VM data representations remain in `data`.

## debugging

- Definition: VM diagnostics, backtraces, terminal interaction, profiling, and debugger behavior.
- Package: `cl-cc/vm/debugging`
- Key types: `vm-crash-report`
- Key functions: `vm-print-backtrace`, `vm-debug`, `vm-terminal`
- Other contexts: None.

## interop

- Definition: Adapters for calling and exposing host functions, regular expressions, FFI, and sandboxed integrations.
- Package: `cl-cc/vm/interop`
- Key types: `vm-bridge-callable`
- Key functions: `vm-bridge-callable`, `vm-bridge-call`
- Other contexts: Host package behavior remains outside the VM and crosses this adapter boundary.
