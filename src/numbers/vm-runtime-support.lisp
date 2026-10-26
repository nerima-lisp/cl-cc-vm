(in-package #:cl-cc/vm/numbers)

;;; ── FR-1112: Numeric helpers ──────────────────────────────────────────────

(defun clamp (value min max)
  "Clamp VALUE into the inclusive range [MIN, MAX]."
  (min max (max min value)))

(defun wrap (value min max)
  "Wrap VALUE into the half-open numeric interval [MIN, MAX)."
  (let ((width (- max min)))
    (when (zerop width)
      (error "Cannot wrap with identical bounds: ~S" min))
    (+ min (mod (- value min) width))))

(defun lerp (start end amount)
  "Linearly interpolate between START and END by AMOUNT."
  (+ start (* amount (- end start))))

(eval-when (:load-toplevel :execute) (vm-register-host-bridge 'clamp #'clamp) (vm-register-host-bridge 'wrap #'wrap) (vm-register-host-bridge 'lerp #'lerp) (vm-register-host-bridge 'signum #'signum))
