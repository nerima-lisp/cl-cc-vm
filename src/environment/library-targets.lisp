(in-package #:cl-cc/vm/environment)

(defvar *compile-shared* nil)

;; FR-897: Static Library Compilation (.a)
(defvar *compile-static* nil)

;; FR-898: Kernel Module Support
(defvar *target-kernel-module* nil)

;; FR-899: Unikernel / Library OS Target
(defvar *target-unikernel* nil)
