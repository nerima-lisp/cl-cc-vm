(in-package #:cl-cc/vm/debugging)

(defun extract-docstrings (path) (declare (ignore path)) nil)

(defun generate-api-docs (path format) (declare (ignore path format)) t)

;; FR-903: Example Code Testing / Doctest
(defun run-doctests (path) (declare (ignore path)) 0)

;; FR-904: Type Signature Documentation
(defun show-types (path) (declare (ignore path)) nil)

;; FR-905: Assertion Density Analysis
(defun analyze-assertion-density (path) (declare (ignore path)) (list :density 0.0))
