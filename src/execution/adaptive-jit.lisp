(in-package #:cl-cc/vm/execution)

(defvar *adaptive-jit-enabled* t
  "When T, the runtime adaptively tiers methods up based on hotness.")

(defvar *jit-tier1-threshold* 100
  "Invocation count before a function is promoted to tier-1 JIT.")

(defvar *jit-compilation-queue* nil
  "Priority queue of (hotness . tier) pairs for deferred JIT compilation.")

(defun enqueue-jit-compilation (tier &key (hotness 1))
  "Enqueue TIER for JIT compilation with HOTNESS priority."
  (push (cons hotness tier) *jit-compilation-queue*)
  (setf *jit-compilation-queue*
        (sort *jit-compilation-queue* #'> :key #'car)))

(defun dequeue-jit-compilation ()
  "Dequeue the highest-hotness pending JIT compilation. Returns (hotness . tier)."
  (pop *jit-compilation-queue*))
