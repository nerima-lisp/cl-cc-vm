# Changelog

## Unreleased

- Share `VM-ARGS`, `VM-HANDLER-LABEL`, `VM-HANDLER-RESULT-REG`, `VM-INDEX`, `VM-VALUE`, and `VM-VALUE-REG` through `cl-cc/vm/protocols` to preserve generic identity and reader behavior.
- Re-export 13 VM symbols used by PHP and JavaScript consumers: `%vm-vector-instance-p`, `%vm-slot-vector-index`, `copy-instance`, `class-slots`, `%vm-hashlike-storage`, `*unbound-slot-marker*`, `%vm-fixed-slot-layout-p`, `%vm-raw-allocate-instance`, `vm-current-closure-stack`, `%vm-closure-object-p`, `vm-closure-dispatch-tag`, `%vm-call-closure-sync`, and `%vm-runtime-callable`.
