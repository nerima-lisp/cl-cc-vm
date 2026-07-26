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

## Development

```sh
nix develop
nix flake check
```

## License

MIT
