# API

Every symbol below is exported from the `:cl-cc/vm` package. The export lists
themselves are organised by domain across `src/exports-instructions.lisp`,
`src/exports-instructions-constructors-core.lisp`,
`src/exports-instructions-constructors-io.lisp`, `src/exports-runtime.lisp`,
`src/exports-runtime-objects.lisp`, `src/exports-opcodes.lisp` and
`src/exports-conditions.lisp`.

This page covers the surface an out-of-tree consumer builds against: programs,
execution, state, the instruction structures a compiler pass constructs and
dispatches on, and serialisation. The full instruction set runs to several
hundred `make-vm-*` constructors, which are listed by category here rather than
one by one; each follows the same shape as the ones spelled out below. Why the
VM is structured this way is in [Home](../index.md); how to run a first program
is in [Getting Started](../getting-started.md).

## Programs and execution

### `vm-program`

Structure holding a compiled program: its instructions and the register its
result is read from.

### `make-vm-program`

Constructor for `vm-program`. Takes `:instructions` (a list of instruction
structures) and `:result-register` (a register keyword such as `:r0`).

### `vm-program-instructions`

Reader for the instruction list of a `vm-program`.

### `vm-program-result-register`

Reader for the register a `vm-program`'s result is taken from.

### `vm-program-calling-convention`

Reader for the program-level calling convention;
`vm-program-function-conventions` holds the per-function overrides.

### `run-compiled`

`(run-compiled program &key output-stream state gc-heap)`. Builds the label
table, coerces the instructions to a vector, executes them, and returns the
value in the program's `:result-register`. Pass `:state` to reuse an existing
state across calls, as a REPL does; pass `:gc-heap` to run under a managed heap
from `cl-cc-runtime`.

### `run-program-slice`

`(run-program-slice instructions labels start-pc state &key gc-heap)`. Executes
an instruction vector from `start-pc` against an existing label table and
state. This is the lower-level entry point `run-compiled` is built on, and the
one to use when the caller already owns the label table.

### `run-compiled-with-io`

`(run-compiled-with-io program &key output-stream input-stream)`. Same as
`run-compiled`, with both directions of the guest's standard streams
redirectable.

### `execute-instruction`

Generic function, specialised on each instruction structure type:
`(execute-instruction instruction state pc labels)`. It returns the next
program counter as its primary value. Defining a new instruction means adding a
method here.

### `vm-print-backtrace`

`(vm-print-backtrace state &key labels stream)`. Prints the VM call stack held
in `state`. Defaults to `*error-output*`.

## State

### `vm-state`

The VM execution state class: registers, heap, call stack, closure environment,
and the deoptimisation bookkeeping.

### `make-vm-state`

`(make-vm-state &key output-stream)`. Creates the canonical public execution
state.

### `make-vm-instance`

Creates an independent VM instance with an optional read-only parent
environment, so several instances can share globals, functions and symbols
without sharing mutable state.

### `vm-parent-environment`

The read-only environment shared between instances.
`vm-parent-environment-globals`, `vm-parent-environment-functions` and
`vm-parent-environment-symbols` read its three tables.

### `vm-state-registers`

Reader for a state's register file.

### `vm-state-heap`

Reader for a state's heap table.

### `vm-reg-get`

Generic function `(vm-reg-get state register)` returning the value in a
register.

### `vm-reg-set`

Generic function `(vm-reg-set state register value)` storing a value into a
register.

### `vm-heap-alloc`

`(vm-heap-alloc state object)`. Allocates `object` on the state's heap and
returns its address.

### `vm-heap-get`

`(vm-heap-get state address)`. Reads the object at a heap address.

### `vm-heap-set`

`(vm-heap-set state address object)`. Writes an object to a heap address.

### `vm-call-stack`

Reader for the state's call stack; `vm-method-call-stack` is the corresponding
generic-function dispatch stack.

### `vm-closure-env`

Reader for the closure environment currently in scope.

## Serialisation

### `instruction->sexp`

Generic function turning an instruction structure into its s-expression form,
for example `(:fadd :r0 :r1 :r2 :f32)`.

### `sexp->instruction`

Generic function reading an s-expression back into an instruction structure.
It round-trips with `instruction->sexp`, and it accepts legacy forms that omit
later-added fields — a float s-expression with no precision element reads back
as `:f64`.

### `define-vm-instruction`

Macro defining an instruction as a `defstruct` with `instruction->sexp` and
`sexp->instruction` support generated from the declaration. Slot definitions
are `(slot-name initform &key reader type)`; `(:sexp-tag :keyword)` sets the
serialised tag and `(:sexp-slots ...)` sets the field order.

## Instruction types and accessors

Instruction structures form a hierarchy rooted at `vm-instruction`, with
`vm-binop` grouping the two-operand arithmetic forms. A compiler pass dispatches
on these type names and reads the operands through the accessors below, which is
why they are external rather than internal: an out-of-tree pass cannot reach an
internal symbol.

### `vm-instruction`

The root instruction structure every other instruction includes.

### `vm-const`

Load a constant into a register. Built with `make-vm-const`, taking `:dst` and
`:value`.

### `vm-move`

Copy one register to another. Built with `make-vm-move`.

### `vm-add`

Generic addition. Built with `make-vm-add`, taking `:dst`, `:lhs` and `:rhs`.
`vm-integer-add`, `vm-integer-sub`, `vm-integer-mul` and the `*-checked`
variants are the fixnum-typed forms.

### `vm-binop`

The parent structure of the two-operand arithmetic instructions.

### `vm-call`

A call instruction. Built with `make-vm-call`.

### `vm-jump-zero`

Conditional branch taken when the tested register is zero. Built with
`make-vm-jump-zero`.

### `vm-label`

A branch target. Built with `make-vm-label`; `run-compiled` collects these into
the label table before execution starts.

### `vm-halt`

Stops execution. Built with `make-vm-halt`, taking `:reg`.

### `vm-cons`

Allocate a cons cell. Built with `make-vm-cons`, taking `:dst`, `:car-src` and
`:cdr-src`. `vm-hash-cons` is the hash-consing variant.

### `vm-car`

Read the car of a cons. Built with `make-vm-car`, taking `:dst` and `:src`.
`vm-cdr` is the corresponding cdr read.

### `vm-rplaca`

Destructively set the car of a cons. Built with `make-vm-rplaca`, taking
`:cons` and `:val`. It routes through the runtime write barrier, so it is
correct under a concurrent collector. `vm-rplacd` is the cdr counterpart.

### `vm-make-closure`

Allocate a closure capturing the named registers. Built with
`make-vm-make-closure`; `vm-closure-object` is the resulting runtime object,
and `vm-closure-entry-label`, `vm-closure-params`, `vm-closure-captured-regs`
and `vm-closure-captured-vals` read it.

### `vm-dst`

Reader for the destination register of an instruction.

### `vm-lhs`

Reader for the left operand register. `vm-rhs` is the right operand and
`vm-src` the single-source-operand form.

### `vm-float-precision`

Reader for the precision of a floating-point instruction, `:f32` or `:f64`. It
defaults to `:f64` when a constructor is called without `:precision`, and a
constructor rejects any other value with a `type-error`.

### `vm-slot-read-dst`

Reader for the destination register of a slot read.

### `vm-get-global-name`

Reader for the name a global-variable read refers to.

## Instruction constructors by category

Every instruction type has a `make-vm-<name>` constructor taking its operand
registers as keyword arguments. They are grouped across two files:

- `exports-instructions-constructors-core.lisp` — arithmetic (integer, checked,
  floating-point, `make-vm-fma`, `make-vm-sqrt`), collections (cons, array,
  hash table, string), characters, and dispatch.
- `exports-instructions-constructors-io.lisp` — the runtime, I/O, string,
  symbol and stream builders, plus control flow (`make-vm-label`,
  `make-vm-jump-zero`).

## Opcodes

The register machine also has a flat opcode encoding, used by the compiled
bytecode path rather than by the structure-based interpreter.

### `+op2-*+`

Opcode constants, one per operation in the flat encoding — `+op2-const+`,
`+op2-add2+`, `+op2-jump-if-nil+`, `+op2-return+` and the rest.

### `+vm-register-count+`

The number of registers the opcode machine's flat register file holds.

### `*opcode-dispatch-table*`

The dispatch table the opcode machine indexes by opcode. The encoder and
opcode-name tables are exported alongside it.

### `make-vm2-state`

Creates the opcode machine's execution state, which is distinct from
`vm-state`. `vm2-state-p`, `vm2-state-registers`, `vm2-state-values-buffer`,
`vm2-state-output-stream` and `vm2-state-global-vars` read it, and `vm-reg-get`
and `vm-reg-set` are specialised on it as well. The state type itself and the
flat-vector helpers around it are deliberately not exported.

## Deoptimisation and tiering

The VM records enough information to reconstruct an interpreter frame from
optimised code, which is what lets a tier-1 entry bail out mid-execution.

### `vm-deopt-info`

Structure describing a deoptimisation point: `vm-deopt-info-pc`,
`vm-deopt-info-label`, `vm-deopt-info-live-regs`, `vm-deopt-info-vreg->preg`,
`vm-deopt-info-inline-stack`, `vm-deopt-info-env` and
`vm-deopt-info-description` read its fields.

### `vm-deopt-frame`

The reconstructed frame itself, with readers for its program counter, reason,
registers, physical registers, call stack, closure environment, values list and
inline stack.

### `vm-capture-deopt-frame`

Captures the current state as a `vm-deopt-frame`.

### `vm-trigger-deopt`

Bails out of optimised code, reconstructing the interpreter frame.

### `vm-reconstruct-interpreter-frame`

Rebuilds an interpreter frame from a captured `vm-deopt-frame`.

### `vm-register-tier1-osr-entry`

Registers an on-stack-replacement entry point for a tier-1 compilation.
`vm-materialize-osr-frame`, `vm-compile-osr-entry-if-hot` and
`vm-osr-stub-enter` drive the rest of the OSR path.

### `*deopt-enabled*`

When false, deoptimisation is disabled. `*osr-enabled*` is the corresponding
switch for on-stack replacement.

## Forward references

### `vm-forward-reference-cell`

A placeholder for a name referenced before it is defined.
`make-vm-forward-reference-cell`, `vm-forward-reference-cell-p`,
`vm-forward-reference-cell-name` and `vm-forward-reference-cell-ref` are its
constructor, predicate and readers.

### `vm-declare-forward-reference`

Declares that a name will be defined later.

### `vm-resolve-forward-references`

Resolves outstanding forward references.
`*vm-forward-reference-auto-resolve-enabled*` controls whether that happens
automatically.

## Conditions

The VM implements the guest's condition system rather than delegating to the
host's, so these are the guest-visible operators.

### `vm-condition`

The root condition type. `vm-serious-condition` and the more specific types
below it form the guest condition hierarchy.

### `vm-define-condition`

Defines a guest condition type.

### `vm-handler-case`

The guest's `handler-case`.

### `vm-signal-condition`

Signals a guest condition.

### `vm-find-handler`

Looks up the handler for a condition in the guest handler stack.
`vm-get-handler-stack`, `vm-push-handler-to-stack`,
`vm-pop-handler-from-stack` and `vm-clear-condition-context` manage that stack.

### `vm-add-restart`

Establishes a restart. `vm-find-restart` and `vm-get-restarts` query them, and
`*active-restarts*` holds the current set.

### `format-rich-condition`

Renders a condition as human-readable text. `format-condition-json` renders the
same information as JSON and `format-stack-trace` renders the backtrace.

### `vm-did-you-mean`

Suggests a near-miss name for an unbound symbol, using
`vm-levenshtein-distance`.

### `*debugger-hook*`

The guest's debugger hook. `*break-on-signals*` and `invoke-debugger` complete
the entry into the guest debugger; the VM shadows these names rather than
reusing the host's.
