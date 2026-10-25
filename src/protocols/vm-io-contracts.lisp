(in-package :cl-cc/vm/protocols)
(defgeneric vm-io-initial-globals ()
  (:documentation "Return the default guest global bindings for the I/O feature."))

;;; ── Canonical VM state access surface ─────────────────────────────────────

(defgeneric vm-create-io-state (output-stream)
  (:documentation "Create an I/O-capable VM state with OUTPUT-STREAM."))
