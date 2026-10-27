# Changelog

## Unreleased

- Add `*VM-GUEST-FUNCTION-PACKAGE*` (default `:cl-user`) to select the guest package whose function symbols the VM host resolves and registers.
- Share `VM-ARGS`, `VM-HANDLER-LABEL`, `VM-HANDLER-RESULT-REG`, `VM-INDEX`, `VM-VALUE`, and `VM-VALUE-REG` through `cl-cc/vm/protocols` so VM subsystems use the same symbol objects and package-qualified forms resolve consistently.
- Expose 13 additional VM symbols through the public `cl-cc/vm` facade for PHP and JavaScript consumers: `%vm-vector-instance-p`, `%vm-slot-vector-index`, `copy-instance`, `class-slots`, `%vm-hashlike-storage`, `*unbound-slot-marker*`, `%vm-fixed-slot-layout-p`, `%vm-raw-allocate-instance`, `vm-current-closure-stack`, `%vm-closure-object-p`, `vm-closure-dispatch-tag`, `%vm-call-closure-sync`, and `%vm-runtime-callable`.
