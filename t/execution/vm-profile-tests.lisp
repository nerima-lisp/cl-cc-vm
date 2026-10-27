(in-package #:cl-cc/vm/execution/test)

(it-sequential "fr-316-vm-call-counts-and-times"
  (let ((state (cl-cc/vm/execution:make-vm-state)))
    (setf (cl-cc/vm/execution:vm-profile-enabled-p state) t)
    (setf (cl-cc/vm/execution:vm-profile-call-stack state) (list "<toplevel>"))
    (setf (cl-cc/vm/execution:vm-profile-call-start-times state) (list 0))
    (let ((now 1000)
          (original-now
           (symbol-function 'cl-cc/vm/execution::%vm-profile-now-ns)))
      (unwind-protect
          (progn
            (setf (symbol-function 'cl-cc/vm/execution::%vm-profile-now-ns) (lambda
                                                                                ()
                                                                              (prog1
                                                                                now
                                                                                (incf
                                                                                 now
                                                                                 4000))))
            (cl-cc/vm/execution::vm-profile-enter-call state
                                                       'fr-316-profiled-function)
            (cl-cc/vm/execution::vm-profile-return state))
        (setf (symbol-function 'cl-cc/vm/execution::%vm-profile-now-ns) original-now)))
    (expect
     (= 1
        (gethash 'fr-316-profiled-function
                 (cl-cc/vm/execution:vm-get-profile-call-counts state)
                 0))
     :to-be-truthy)
    (expect
     (= 4000
        (gethash 'fr-316-profiled-function
                 (cl-cc/vm/execution:vm-get-profile-call-times state)
                 0))
     :to-be-truthy)))
