(in-package #:cl-cc/vm/execution)

(defmacro as-pattern (whole pattern) `(let ((,whole ,pattern)) ,whole))
