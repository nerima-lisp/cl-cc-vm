# Getting Started

This page adds `cl-cc-vm` to a project and runs one program through the VM
from start to finish.

## Add the flake input

`cl-cc-vm` is consumed as a Nix flake input. Pin it to a release tag: a bare
`github:nerima-lisp/cl-cc-vm` follows the default branch, so an unrelated push
upstream would change your build without warning.

```nix
{
  inputs.cl-cc-vm = {
    url = "github:nerima-lisp/cl-cc-vm/v0.1.0";
    flake = false;
  };
}
```

`cl-cc-vm` is a plain ASDF source tree, so it is fetched with `flake = false`
and put on `CL_SOURCE_REGISTRY` alongside its own dependencies. Its runtime
dependencies are `cl-cc-bootstrap`, `cl-cc-runtime`, `cl-regex-kit` and
`cl-tty-kit`; `flake.nix` in this repository shows the full closure, each pin
with a comment explaining why it is pinned where it is.

## Add the ASDF dependency

```lisp
:depends-on ("cl-cc-vm")
```

Then load it:

```lisp
(asdf:load-system "cl-cc-vm")
```

All exported symbols live in the `:cl-cc/vm` package. The examples below
qualify every one of them so it is clear which symbol comes from where.

## Build and run a program

A VM program is a `vm-program` structure carrying a list of instruction
structures and the register whose value is the program's result. Each
instruction is built by its own `make-vm-*` constructor, and registers are
named by keyword.

Cons two values, take the `car` of the pair, and halt on it:

```lisp
(let ((program
        (cl-cc/vm:make-vm-program
         :instructions
         (list (cl-cc/vm:make-vm-const :dst :r0 :value 11)
               (cl-cc/vm:make-vm-const :dst :r1 :value 22)
               (cl-cc/vm:make-vm-cons :dst :r2 :car-src :r0 :cdr-src :r1)
               (cl-cc/vm:make-vm-car :dst :r3 :src :r2)
               (cl-cc/vm:make-vm-halt :reg :r3))
         :result-register :r3)))
  (cl-cc/vm:run-compiled program))
;; => 11
```

`run-compiled` builds the label table, allocates a `vm-state`, executes the
instruction vector, and returns the value in `:result-register`. That is the
whole entry point for the common case — there is no separate assemble step.

## Run under a managed heap

`run-compiled` accepts a `:gc-heap` argument. Pass a heap from `cl-cc-runtime`
when the program allocates managed objects, and the executor services pending
collections at safepoints as it runs:

```lisp
(let ((heap (cl-cc/runtime:make-rt-heap :young-size 32 :old-size 32))
      (program (cl-cc/vm:make-vm-program
                :instructions (list (cl-cc/vm:make-vm-const :dst :r0 :value 42)
                                    (cl-cc/vm:make-vm-halt :reg :r0))
                :result-register :r0)))
  (cl-cc/runtime:rt-gc-request heap)
  (cl-cc/vm:run-compiled program :gc-heap heap))
;; => 42
```

## Serialise an instruction

Every instruction round-trips through an s-expression form, which is what the
compiler's own passes and the on-disk bytecode format use:

```lisp
(cl-cc/vm:instruction->sexp
 (cl-cc/vm:make-vm-float-add :dst :r0 :lhs :r1 :rhs :r2 :precision :f32))
;; => (:FADD :R0 :R1 :R2 :F32)

(cl-cc/vm:vm-float-precision
 (cl-cc/vm:sexp->instruction '(:fadd :r0 :r1 :r2)))
;; => :F64
```

A float instruction written without an explicit precision defaults to `:f64`,
and a legacy s-expression with no precision element reads back as `:f64`, so
older bytecode keeps its meaning.

## Build and test this repository

```sh
nix develop                   # development shell with SBCL and the registry set
nix flake check               # tests, formatting, and the docs build
nix run .#test                # the same suite nix flake check runs
nix build .#coverage-report   # sb-cover HTML report in result/
```

`nix build .#coverage-report` runs sb-cover in the same sandboxed `$HOME` and
`$TMPDIR` the test check uses. That is the reliable way to run it: sb-cover
needs the instrumentation proclamation in place before the system is
force-recompiled, and an interactive `nix develop -c sbcl ...` invocation has
been observed to stall outside that sandbox for reasons specific to the host
shell. Read `result/cover-index.html` to find gaps.

Development happens on Linux. Since the 2026-08-01 revision the flake declares
`systems = [ "x86_64-linux" ]` and nothing else, so `nix develop` and
`nix build` have no outputs to produce on macOS.

## Next

The [API reference](reference/api.md) lists the exported instruction types,
their constructors and accessors, and the execution entry points.
