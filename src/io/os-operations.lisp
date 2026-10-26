(in-package #:cl-cc/vm/io)

(defun sendfile (dst src count) (declare (ignore dst src count)) 0)

;; FR-820: UNIX Signal Handling
(defun handle-signal (sig handler) (declare (ignore sig handler)) t)

;; FR-821: Shared Memory IPC
(defun shm-open (name &key create size) (declare (ignore name create size)) nil)
