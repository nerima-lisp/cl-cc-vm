(in-package :cl-cc/vm/protocols)

;;; ── VM Label-Table Contract ──────────────────────────────────────────────

(defgeneric build-label-table (instructions)
  (:documentation "Build the VM label table for INSTRUCTIONS."))
