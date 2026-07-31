# cl-cc-vm

The bytecode virtual machine for the [cl-cc](https://github.com/nerima-lisp/cl-cc)
Common Lisp compiler: instruction set, executor, I/O, CLOS, the condition
system, and collections.

## Why this is a separate repository

cl-cc's split design had ruled the VM out of scope, on the grounds that the core
is a self-referential web, citing 301 cross-package `cl-cc/vm::` references.

That count points the wrong way. It measures other packages reaching *into* the
VM's internals — a public API problem, and the one §5-2 of that design is about —
not the VM depending on anything. `cl-cc/optimize` and `cl-cc/codegen` reached
zero such references before this extraction.

What the VM actually depends on is what its `.asd` declares: `cl-cc-bootstrap`
and `cl-cc-runtime`. Loading it leaves `cl-cc/expand`, `cl-cc/compile`,
`cl-cc/optimize`, `cl-cc/parse`, `cl-cc/ast` and `cl-cc/type` unloaded. `t/`
asserts exactly that, because it is the property the extraction rests on.

## Usage

```lisp
(asdf:load-system "cl-cc-vm")
```

## Dependencies

Beyond `cl-cc-bootstrap` and `cl-cc-runtime`, two nerima-lisp packages are
used directly, with no hand-rolled reimplementation and no intermediate
wrapper layer:

- The VM's guest-visible regex stdlib (`src/regex.lisp`) is
  [`cl-regex-kit`](https://github.com/nerima-lisp/cl-regex-kit).
- Terminal raw-mode entry/exit and terminal-size queries
  (`src/vm-terminal.lisp`) are
  [`cl-tty-kit`](https://github.com/nerima-lisp/cl-tty-kit)'s termios/ioctl
  calls, replacing what used to shell out to `stty(1)`.

Test-only dependencies ([`cl-weave`](https://github.com/nerima-lisp/cl-weave)
and its transitive `cl-nix-forge`/`treefmt-nix`) and the rest of the
dependency closure are pinned in `flake.nix`, each with a comment on why
that pin.

## Development

```sh
nix develop
nix flake check
```

## Testing and coverage

```sh
nix run .#test                # same suite nix flake check runs
nix build .#coverage-report   # sb-cover expression/branch HTML report in result/
```

`coverage-report` measures `src/` coverage under the current `t/` suite but
does not gate on a threshold: `t/vm-boundary-test.lisp` asserts the
dependency-closure/public-surface properties this extraction rests on, and
the rest of `t/` covers specific subsystems (the guest-visible regex
stdlib, native bignum arithmetic, the native FORMAT renderer, terminal
control) rather than the whole VM, so a hard coverage floor here would be a
fiction. Read `result/cover-index.html` to find real gaps.

`nix build` runs it in the same sandboxed `$HOME`/`$TMPDIR` the `default`
check uses, which is the reliable way to run it: `sb-cover` requires
`(proclaim '(optimize (sb-cover:store-coverage-data 3)))` before the system
is force-recompiled (see `run-coverage.lisp`), and outside that sandbox an
interactive `nix develop -c sbcl ...` invocation has been observed to stall
indefinitely for reasons specific to the host shell.

As of this measurement: 12.4% expression / 12.2% branch coverage. `t/` is
still nine narrowly-scoped files against a ~25,000-line VM -- most of the
gap is stdlib surface (I/O, symbols, packages, generic-function dispatch)
with no direct test yet, not a measurement problem. Growing it is a
matter of adding more cases in the same two shapes already established:
call an internal helper directly (`t/vm-numeric-bignum-test.lisp`) when
one exists, or build a small `vm-program` of `make-vm-*` instructions and
check `run-compiled`'s result (`t/vm-list-execute-test.lisp`,
`t/vm-array-execute-test.lisp`, `t/vm-hash-execute-test.lisp`,
`t/vm-string-execute-test.lisp`, `t/vm-clos-execute-test.lisp`) when the
behavior only exists at the instruction level. A single well-chosen
`run-compiled` case can move the aggregate more than a narrow one: the
CLOS class-definition/instance/slot-access case above alone raised
expression coverage by 1.8 points by touching several large subsystems
(`vm-clos-execute.lisp`, `vm-clos-slots*.lisp`) at once.

## License

MIT
