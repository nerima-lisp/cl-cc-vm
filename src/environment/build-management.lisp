(in-package #:cl-cc/vm/environment)

(defun generate-lockfile (deps) (declare (ignore deps)) "cl-cc.lock")

;; FR-814: Dependency Vulnerability Scanning (src/build/security-scan.lisp)
(defun audit-dependencies () nil)

;; FR-815: Build-Time Source Code Generation (src/build/codegen-step.lisp)
(defun generate-from-schema (schema target) (declare (ignore schema target)) t)
