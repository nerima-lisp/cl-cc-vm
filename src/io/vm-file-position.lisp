(in-package #:cl-cc/vm/io)

(defun vm-file-position (stream &optional (position nil position-p))
  "FR-868: Get or set file position on STREAM.

POSITION accepts ANSI FILE-POSITION values (a non-negative integer, :START, or
:END).  The CL-CC extension :CURRENT performs a no-op seek at the current
position and returns the normal setter boolean."
  (if position-p
      (if (eq position :current)
          (let ((current (file-position stream)))
            (and current (if (file-position stream current) t nil)))
          (if (file-position stream position) t nil))
      (file-position stream)))
