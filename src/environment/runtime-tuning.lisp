(in-package #:cl-cc/vm/environment)

(defun vm-record-gc-pause (pause-ms)
  "Record a GC pause of PAUSE-MS milliseconds; adaptively shrink nursery if pause is long."
  (when (> pause-ms 50.0d0)
    (flet ((shrink-sym (pkg-name sym-name)
             (let ((pkg (find-package pkg-name)))
               (when pkg
                 (let ((sym (find-symbol sym-name pkg)))
                   (when (and sym (boundp sym))
                     (setf (symbol-value sym)
                           (max (floor (symbol-value sym) 2) 4096))))))))
      (shrink-sym "CL-CC/RUNTIME" "*GC-NURSERY-SIZE*")
      (shrink-sym "CL-CC/RUNTIME" "*GC-YOUNG-SIZE-WORDS*"))))

(defun vm-handle-runtime-flag (flags)
  "Process a list of runtime flag strings, e.g. '(\"--adaptive-jit\")."
  (dolist (flag flags)
    (cond
      ((string= flag "--adaptive-jit")
       (setf *adaptive-jit-enabled* t))
      ((string= flag "--no-adaptive-jit")
       (setf *adaptive-jit-enabled* nil)))))

(defun %rt-sym-value (pkg-name sym-name)
  (let* ((pkg (find-package pkg-name))
         (sym (and pkg (find-symbol sym-name pkg))))
    (and sym (boundp sym) (symbol-value sym))))

(defun runtime-tuning-report ()
  "Return a plist describing current runtime tuning state."
  (list :jit-tier1-threshold *jit-tier1-threshold*
        :adaptive-jit-enabled *adaptive-jit-enabled*
        :gc-nursery-size (%rt-sym-value "CL-CC/RUNTIME" "*GC-NURSERY-SIZE*")
        :gc-young-size-words (%rt-sym-value "CL-CC/RUNTIME" "*GC-YOUNG-SIZE-WORDS*")))
