(in-package #:cl-cc/vm/execution)

(defvar *vm-current-labels* nil
  "Dynamically bound to the current VM label table while invoking host bridge callables.")

(defvar *vm-current-dst-reg* nil
  "Dynamically bound to the destination register while invoking host bridge callables.")

(defvar *vm-current-pc* nil
  "Dynamically bound to the continuation PC while invoking host bridge callables.")
