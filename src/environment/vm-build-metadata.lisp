(in-package #:cl-cc/vm/environment)

;; NOTE: FR-895 (freeze/thaw symbol table) and FR-896 (package lock)
;; are already implemented in vm.lisp lines 841-965 with superior
;; implementations (binary search compact vector, weak references).
;; Do NOT override those definitions.

;; ── FR-917: Reproducible Build Support ─────────────────────────────────

(defconstant +unix-to-universal-time-offset+ 2208988800
  "Seconds between the Common Lisp and Unix epochs.")

(defparameter *build-seed* nil
  "When non-NIL, fixed seed for reproducible builds; NIL preserves defaults.")

(defparameter *deterministic-hash-table-seed* nil
  "Stable hash seed used by deterministic hash helpers.")

(defun %parse-non-negative-integer (value)
  (when value
    (handler-case
        (let ((n (parse-integer value :junk-allowed nil)))
          (and (not (minusp n)) n))
      (error () nil))))

(defun source-date-epoch ()
  "Return SOURCE_DATE_EPOCH as Unix seconds, or NIL when unset/invalid."
  (%parse-non-negative-integer (host-kit:getenv "SOURCE_DATE_EPOCH")))

(defun build-timestamp ()
  "Return build timestamp as universal-time, honoring SOURCE_DATE_EPOCH."
  (let ((epoch (source-date-epoch)))
    (if epoch
        (+ epoch +unix-to-universal-time-offset+)
        (cl:get-universal-time))))
