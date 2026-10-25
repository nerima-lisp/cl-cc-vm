(in-package #:cl-cc/vm/debugging)

(defun repl-complete (prefix) (declare (ignore prefix)) nil)

;; FR-880: REPL Syntax Highlighting
(defvar *repl-syntax-colors* nil)

;; FR-881: REPL Undo / Side-Effect Journal
(defvar *repl-journal* nil)

(defun repl-undo () (when *repl-journal* (pop *repl-journal*)) t)
