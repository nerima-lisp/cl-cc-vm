(in-package #:cl-cc/vm/execution)

;; FR-860: Pass Pipeline Configuration
(defvar *pass-pipeline* nil)

;; FR-861: Analysis Preservation Tracking
(defun preserve-analysis (pass analysis) (declare (ignore pass analysis)) t)

;; FR-863: Loop Versioning with Runtime Alias Checks
(defun version-loops-with-alias-checks (loop) (declare (ignore loop)) loop)
