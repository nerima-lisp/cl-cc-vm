(in-package #:cl-cc/vm/debugging/test)

(defun register-feature-api-tests ()
  nil
  (describe-sequential "debugging feature API"
                       (it "enables step prompts while evaluating a form"
                           (let* ((input
                                   (make-string-input-stream (format nil "~%")))
                                  (output (make-string-output-stream))
                                  (*query-io*
                                   (make-two-way-stream input output)))
                             (expect
                              (cl-cc/vm/debugging:vm-step
                               (quote (cl-cc/vm/debugging:vm-step-prompt)))
                              :to-be
                              nil)
                             (expect
                              (not
                               (null
                                (search "Step"
                                        (get-output-stream-string output))))
                              :to-be
                              t)))))
