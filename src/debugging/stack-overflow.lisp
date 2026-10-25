(in-package #:cl-cc/vm/debugging)

(defun report-stack-overflow (depth call-chain)
  "Report a stack overflow at DEPTH with CALL-CHAIN information."
  (error 'simple-error
         :format-control "Stack overflow at depth ~D: call chain ~S"
         :format-arguments (list depth call-chain)))
