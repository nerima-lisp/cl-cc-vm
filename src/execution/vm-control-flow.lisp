(in-package #:cl-cc/vm/execution)

(defmethod execute-instruction ((inst vm-label) state pc labels)
  (declare (ignore state labels))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-jump) state pc labels)
  (declare (ignore state pc))
  (values (vm-label-table-lookup labels (vm-label-name inst)) nil nil))

(defun vm-falsep (value)
  "Return T if VALUE is falsy.

The cl-cc execution model treats both NIL and numeric zero as false."
  (or (null value)
      (and (numberp value)
           (zerop value))))

(defmethod execute-instruction ((inst vm-jump-zero) state pc labels)
  (if (vm-falsep (vm-reg-get state (vm-reg inst)))
      (values (vm-label-table-lookup labels (vm-label-name inst)) nil nil)
      (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-select) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (if (vm-falsep (vm-reg-get state (vm-select-cond-reg inst)))
                  (vm-reg-get state (vm-select-else-reg inst))
                  (vm-reg-get state (vm-select-then-reg inst))))
  (values (1+ pc) nil nil))
