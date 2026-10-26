(in-package #:cl-cc/vm/debugging)

(defun start-cpu-profile () t)

;; FR-807: PMU (src/profiling/pmu.lisp)
(defun read-pmu-counters () nil)
