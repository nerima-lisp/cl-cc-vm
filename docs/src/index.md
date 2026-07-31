# cl-cc-vm

`cl-cc-vm` is the bytecode virtual machine of the
[cl-cc](https://github.com/nerima-lisp/cl-cc) Common Lisp compiler: the
instruction set, the executor that runs it, and the guest-visible runtime that
sits behind it — I/O, CLOS, the condition system, and the collection types.

A program is a `vm-program` holding a list of instruction structures, and
`run-compiled` executes it:

```lisp
(asdf:load-system "cl-cc-vm")

(cl-cc/vm:run-compiled
 (cl-cc/vm:make-vm-program
  :instructions (list (cl-cc/vm:make-vm-const :dst :r0 :value 41)
                      (cl-cc/vm:make-vm-const :dst :r1 :value 1)
                      (cl-cc/vm:make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                      (cl-cc/vm:make-vm-halt :reg :r2))
  :result-register :r2))
;; => 42
```

Everything lives in the `:cl-cc/vm` package.

## Where to go next

- [Getting Started](getting-started.md) — add the system as a dependency and
  run a program end to end.
- [API](reference/api.md) — the exported instruction types, constructors,
  accessors, and the execution entry points.
- [Releases](https://github.com/nerima-lisp/cl-cc-vm/releases) — release
  history. The GitHub Release description is the only canonical changelog.

## Why this is a separate repository

`cl-cc`'s split design had ruled the VM out of scope, on the grounds that the
core is a self-referential web, citing 301 cross-package `cl-cc/vm::`
references. That count measures other packages reaching *into* the VM's
internals, not the VM depending on anything.

What the VM depends on is what its `.asd` declares: `cl-cc-bootstrap` and
`cl-cc-runtime`. Loading it leaves `cl-cc/expand`, `cl-cc/compile`,
`cl-cc/optimize`, `cl-cc/parse`, `cl-cc/ast` and `cl-cc/type` unloaded, and
`t/vm-boundary-test.lisp` asserts exactly that, because it is the property the
extraction rests on.

Two further nerima-lisp packages are used directly, with no wrapper layer:
[`cl-regex-kit`](https://github.com/nerima-lisp/cl-regex-kit) is the
guest-visible regex stdlib, and
[`cl-tty-kit`](https://github.com/nerima-lisp/cl-tty-kit) provides raw-mode
entry/exit and terminal-size queries.

## Project policies

Contribution, conduct, security and support policies are maintained once for
the whole organisation in
[nerima-lisp/.github](https://github.com/nerima-lisp/.github).
