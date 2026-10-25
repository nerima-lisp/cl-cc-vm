(in-package #:cl-cc/vm/execution)

(defstruct (vm-mutex (:constructor make-mutex (&optional (name "mutex"))))
  "FR-847: Mutex for mutual exclusion."
  (name "mutex"))

(defstruct (vm-rwlock (:constructor make-rwlock (&optional (name "rwlock"))))
  "FR-848: Reader-writer lock."
  (name "rwlock"))
